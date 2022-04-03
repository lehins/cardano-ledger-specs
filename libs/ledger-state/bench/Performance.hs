{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Cardano.Binary
import Cardano.Ledger.Address
import Cardano.Ledger.Alonzo
import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.CompactAddress
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era
import Cardano.Ledger.Shelley.API.Mempool
import Cardano.Ledger.Shelley.API.Wallet (getFilteredUTxO, getUTxO)
import Cardano.Ledger.Shelley.Genesis (ShelleyGenesis (..), mkShelleyGlobals)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.State.UTxO
import Cardano.Ledger.TxIn
import Cardano.Slotting.EpochInfo (fixedEpochInfo)
import Cardano.Slotting.Slot
import Cardano.Slotting.Time (mkSlotLength)
import Control.DeepSeq
import Criterion.Main
import Data.Aeson
import Data.Bifunctor (first)
import Data.ByteString.Base16.Lazy as BSL16
import Data.ByteString.Lazy (ByteString)
import Data.Compact.HashMap (Keyed (..))
import qualified Data.Compact.SplitMapOriginal as SplitMap
import Data.Default.Class (def)
import Data.Foldable as F
import Data.IntMap.Strict as IntMap
import Data.Map.Strict as Map
import Data.Set as Set
import System.Environment (getEnv)

-- This instance might be useful again if we can get SplitMap to perform well.
instance CC.Crypto crypto => SplitMap.Split (TxIn crypto) where
  splitKey (TxIn txId txIx) =
    let i = txIxToInt txIx
        k = toKey txId
     in i `seq` k `seq` (i, k)
  {-# INLINE splitKey #-}
  joinKey txIx key =
    -- `fromIntegral` is safe here, since we have only valid values in the SplitMap:
    TxIn (fromKey key) (TxIx (fromIntegral txIx))
  {-# INLINE joinKey #-}

main :: IO ()
main = do
  let ledgerVarName = "BENCH_LEDGER_STATE_PATH"
      genesisVarName = "BENCH_GENESIS_PATH"
  ledgerStateFilePath <- getEnv ledgerVarName
  genesisFilePath <- getEnv genesisVarName
  genesis <- either error id <$> eitherDecodeFileStrict' genesisFilePath

  let toMempoolState :: NewEpochState CurrentEra -> MempoolState CurrentEra
      toMempoolState NewEpochState {nesEs = EpochState {esLState}} =
        (_utxoState esLState, _delegationState esLState)
      pp :: PParams CurrentEra
      pp = def
      !globals = mkGlobals genesis pp
      !slotNo = SlotNo 55733343
      seqTuple :: (a, b) -> (a, b)
      seqTuple (x, y) = x `seq` y `seq` (x, y)
      applyTx' mempoolEnv mempoolState =
        either (error . show) (\(x, y) -> seqTuple (seqTuple x, y))
          . applyTx globals mempoolEnv mempoolState
      reapplyTx' mempoolEnv mempoolState =
        either (error . show) seqTuple
          . reapplyTx globals mempoolEnv mempoolState
  putStrLn $ "Importing NewEpochState from: " ++ show ledgerStateFilePath
  es <- readNewEpochState ledgerStateFilePath
  putStrLn "Done importing NewEpochState"
  let utxo = unUTxO $ getUTxO es
      utxosm = SplitMap.fromMap utxo
  defaultMain
    [ env (pure ((mkMempoolEnv es slotNo, toMempoolState es))) $ \ ~(mempoolEnv, mempoolState) ->
        bgroup
          "reapplyTx"
          [ env (pure validatedTx1) $
              bench "Tx1" . whnf (reapplyTx' mempoolEnv mempoolState),
            env (pure validatedTx2) $
              bench "Tx2" . whnf (reapplyTx' mempoolEnv mempoolState),
            env (pure validatedTx3) $
              bench "Tx3" . whnf (reapplyTx' mempoolEnv mempoolState),
            env
              (pure [validatedTx1, validatedTx2, validatedTx3])
              $ bench "Tx1+Tx2+Tx3" . whnf (F.foldl' (reapplyTx' mempoolEnv) mempoolState)
          ],
      env (pure ((mkMempoolEnv es slotNo, toMempoolState es))) $ \ ~(mempoolEnv, mempoolState) ->
        bgroup
          "applyTx"
          [ env (pure (extractTx validatedTx1)) $
              bench "Tx1" . whnf (applyTx' mempoolEnv mempoolState),
            env (pure (extractTx validatedTx2)) $
              bench "Tx2" . whnf (applyTx' mempoolEnv mempoolState),
            env (pure (extractTx validatedTx3)) $
              bench "Tx3" . whnf (applyTx' mempoolEnv mempoolState)
          ],
      env (pure es) $ \newEpochState ->
        let (_, minTxOut) = Map.findMin utxo
            (_, maxTxOut) = Map.findMax utxo
            setAddr =
              Set.fromList [getTxOutAddr minTxOut, getTxOutAddr maxTxOut]
         in bgroup "MinMaxTxId" $
              [ env (pure setAddr) $
                  bench "getFilteredNewUTxO" . nf (getFilteredUTxO newEpochState),
                env (pure setAddr) $
                  bench "getFilteredOldUTxO" . nf (getFilteredOldUTxO newEpochState)
              ],
      bgroup
        "UTxO"
        [ env (pure (utxo, utxosm, fromMapS utxo)) $ \ ~(m, sm, s) ->
            bgroup "toList" $
              [ bench "Map" $ nf (Map.foldrWithKey' f ()) m,
                bench "SplitMap" $ nf (SplitMap.foldrWithKey' f ()) sm,
                bench "SMap" $ nf (foldrWithKeyS f ()) s
              ]
        ]
    ]
  where
    f _k v () = v `deepseq` ()

type K = TxIn CC.StandardCrypto

type V = TxOut CurrentEra

newtype SplitMap = SplitMap (IntMap.IntMap (Map.Map (TxId CC.StandardCrypto) V))
  deriving (NFData)

insertS :: K -> V -> SplitMap -> SplitMap
insertS (TxIn txId txIx) v (SplitMap imap) =
  SplitMap (IntMap.insertWith Map.union ix (Map.singleton txId v) imap)
  where
    ix = txIxToInt txIx

fromMapS :: Map.Map K V -> SplitMap
fromMapS = Map.foldrWithKey' insertS (SplitMap mempty)

toListS :: SplitMap -> [(K, V)]
toListS = foldrWithKeyS (\k v acc -> (k, v) : acc) []
{-# INLINE toListS #-} -- needed for list fusion, see below

foldrWithKeyS :: (K -> V -> a -> a) -> a -> SplitMap -> a
foldrWithKeyS f ans0 (SplitMap imap) =
  IntMap.foldrWithKey'
  (\ix kmap ans1 -> Map.foldrWithKey' (\key -> f (TxIn key (TxIx (fromIntegral ix)))) ans1 kmap)
  ans0
  imap
{-# INLINE foldrWithKeyS #-}
-- foldrWithKeyS comb ans0 (SplitMap imap) = IntMap.foldrWithKey' comb2 ans0 imap
--   where
--     comb2 ix kmap !ans1 = Map.foldrWithKey' comb3 ans1 kmap
--       where
--         comb3 key v !ans2 = let k = TxIn key (TxIx (fromIntegral ix)) in comb k v ans2
--         {-# INLINE comb3 #-}
--     {-# INLINE comb2 #-}
-- {-# INLINE foldrWithKeyS #-}

-- insertWithKey :: (TxIn StandardCrypto -> V -> V -> V) -> K -> V -> SplitMap -> SplitMap
-- insertWithKey combine (TxIn txId txIx) v (SplitMap imap) =
--   SplitMap (IntMap.insertWith combine2 n (Map.insert key v mempty) imap)
--   where
--     ix = txIxToInt txIx
--     combine2 _km1 km2 = Map.insertWith (combine k) key v km2

decodeTx :: ByteString -> Core.Tx CurrentEra
decodeTx hex = either error id $ do
  bsl <- BSL16.decode hex
  first show $ decodeAnnotator "Tx" fromCBOR bsl

-- | Most basic ada-only transaction:
--
-- * One input with Shelley address without staking
-- * One destination and change back to the address from original input.
validatedTx1 :: Validated (Core.Tx CurrentEra)
validatedTx1 =
  unsafeMakeValidated $
    decodeTx
      "84a500818258201a42ba3e89f7ac4e526a61836b29847cb143c504e1429caa75c9ee06\
      \312f7ef7000d80018282581d61780648b89ea2f11fa9bbdd67552db5dd020eda1c9a54\
      \142dd9f1b1361a001440b3825839011e9c9362752648dda1dfa9382c70aad2bedbb683\
      \c213c6cdf4c4ef14800525c22102c1a6e05589b0209ccc06f57cb332960920d45080ba\
      \c31a000f4240021a0002a2ad0e81581c780648b89ea2f11fa9bbdd67552db5dd020eda\
      \1c9a54142dd9f1b136a10081825820cf2477066091b565f87f0445817c4df726900b29\
      \af3f05d229309afdbf94296d5840028182010e2204fae981f24df1be5e890122d2d854\
      \dde86c57635fa88de1df834b4321e2aaade0353947b682f265cc546abc4db86853f435\
      \842a808e500e4201f5f6"

-- | Slightly less basic ada-only transaction:
--
-- * One input with Shelley address /with/ staking address
-- * One destination and change back to the address from original input.
validatedTx2 :: Validated (Core.Tx CurrentEra)
validatedTx2 =
  unsafeMakeValidated $
    decodeTx
      "84a500818258204c94b067b71c219d178e81d5aa87d2bc8c567855056f646fd244979d\
      \7b989f83000d80018282583901780648b89ea2f11fa9bbdd67552db5dd020eda1c9a54\
      \142dd9f1b13693d02e102e7e1917f14280d263c6878f9a7f238c2a24e702e9ea6ccf1a\
      \0011cae3825839011e9c9362752648dda1dfa9382c70aad2bedbb683c213c6cdf4c4ef\
      \14800525c22102c1a6e05589b0209ccc06f57cb332960920d45080bac31a000f424002\
      \1a0002a77d0e81581c780648b89ea2f11fa9bbdd67552db5dd020eda1c9a54142dd9f1\
      \b136a10081825820cf2477066091b565f87f0445817c4df726900b29af3f05d229309a\
      \fdbf94296d5840634a53d6826e3c63a0fd451e142c565beea8e08ff47bb302b7365a97\
      \5a723618c48ffafe2048ae9181a388bdea5f61ca7f085d1073e91d64722a2e76509c7b\
      \0ef5f6"

-- | Transaction with non-ADA value
--
-- * One input with Shelley address /with/ staking address and some tokens
-- * One destination and change back to the address from original input.
validatedTx3 :: Validated (Core.Tx CurrentEra)
validatedTx3 =
  unsafeMakeValidated $
    decodeTx
      "84a500818258200e5d59af740d8682656ade4af6c069f926b9ec51689ee962260b4127\
      \8d21538e000d80018282583901780648b89ea2f11fa9bbdd67552db5dd020eda1c9a54\
      \142dd9f1b13693d02e102e7e1917f14280d263c6878f9a7f238c2a24e702e9ea6ccf1a\
      \002a26f282583901780648b89ea2f11fa9bbdd67552db5dd020eda1c9a54142dd9f1b1\
      \3693d02e102e7e1917f14280d263c6878f9a7f238c2a24e702e9ea6ccf821a00150bd0\
      \a1581ca89568bb399d0cdc38367e47831c95186f5c79e58174e08a18232396a14a4554\
      \424643546f6b656e1a006cc9f2021a0002afe90e81581c780648b89ea2f11fa9bbdd67\
      \552db5dd020eda1c9a54142dd9f1b136a10081825820cf2477066091b565f87f044581\
      \7c4df726900b29af3f05d229309afdbf94296d584088444a5845b198a2d255175770be\
      \7120c2d3482751b14f06dd41d7ff023eeae6e63933b097c023c1ed19df6a061173c45aa\
      \54cceb568ff1886e2716e84e6260df5f6"

mkGlobals :: ShelleyGenesis CurrentEra -> Core.PParams CurrentEra -> Globals
mkGlobals genesis pp =
  mkShelleyGlobals genesis epochInfoE majorPParamsVer
  where
    majorPParamsVer = pvMajor $ _protocolVersion pp
    epochInfoE =
      fixedEpochInfo
        (sgEpochLength genesis)
        (mkSlotLength $ sgSlotLength genesis)

getFilteredOldUTxO ::
  Era era =>
  NewEpochState era ->
  Set (Addr (Crypto era)) ->
  UTxO era
getFilteredOldUTxO ss addrs =
  UTxO $
    Map.filter (\out -> getTxOutCompactAddr out `Set.member` addrSBSs) fullUTxO
  where
    UTxO fullUTxO = getUTxO ss
    addrSBSs = Set.map compactAddr addrs
