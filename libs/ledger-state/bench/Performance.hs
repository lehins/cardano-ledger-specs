{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Cardano.Ledger.Shelley.API.Wallet

import Data.Aeson
import Cardano.Binary
import Cardano.Ledger.Address
import Cardano.Ledger.Alonzo
import Cardano.Ledger.Alonzo.Tx
import Cardano.Ledger.Alonzo.PParams
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Coin
import Cardano.Ledger.CompactAddress
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era
import Cardano.Ledger.Shelley.API.Mempool
import Cardano.Ledger.Shelley.Genesis (ShelleyGenesis(..), mkShelleyGlobals)
import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Shelley.Rules.Ledger (LedgerEnv (..))
import Cardano.Ledger.Shelley.UTxO (UTxO (..))
import Cardano.Ledger.State.UTxO
import Cardano.Slotting.EpochInfo (EpochInfo, fixedEpochInfo)
import Cardano.Slotting.Slot
import Cardano.Slotting.Time (mkSlotLength)
import Control.Monad
import Criterion.Main
import Data.Bifunctor (first)
import Data.ByteString.Base16.Lazy as BSL16
import Data.ByteString.Lazy (ByteString)
import Data.Default.Class (def)
import Data.Map.Strict as Map
import Data.Set as Set
import System.Environment (getEnv)

decodeTxBody :: ByteString -> Core.TxBody CurrentEra
decodeTxBody hex = either error id $ do
  bsl <- BSL16.decode hex
  first show $ decodeAnnotator "TxBody" fromCBOR bsl

txBody1 :: Core.TxBody CurrentEra
txBody1 =
  decodeTxBody
    "86a50081825820e537c5d7b03acbcf1ba11cece20eef7778fd9d742f0aa614eee133e218ff188\
    \9000d800182825839015c0c49b32c56787f1cc3f9a9b2fd787b8250ba756d28038aedf208c180\
    \0525c22102c1a6e05589b0209ccc06f57cb332960920d45080bac31a01486597825839011e9c9\
    \362752648dda1dfa9382c70aad2bedbb683c213c6cdf4c4ef14800525c22102c1a6e05589b020\
    \9ccc06f57cb332960920d45080bac31a0024c610021a000290f90e809fff8080f5f6"

mkTx :: Core.TxBody CurrentEra -> Validated (Core.Tx CurrentEra)
mkTx txBody =
  unsafeMakeValidated $
    ValidatedTx
      { body = txBody,
        wits = mempty,
        isValid = IsValid True,
        auxiliaryData = SNothing
      }

mkGlobals :: ShelleyGenesis CurrentEra -> Core.PParams CurrentEra -> Globals
mkGlobals genesis pp =
  mkShelleyGlobals genesis epochInfo majorPParamsVer
  where
    majorPParamsVer = pvMajor $ _protocolVersion pp
    epochInfo =
      fixedEpochInfo
        (sgEpochLength genesis)
        (mkSlotLength $ sgSlotLength genesis)

main :: IO ()
main = do
  let ledgerVarName = "BENCH_LEDGER_STATE_PATH"
      genesisVarName = "BENCH_GENESIS_PATH"
  ledgerStateFilePath <- getEnv ledgerVarName
  genesisFilePath <- getEnv genesisVarName
  genesis <- either error id <$> eitherDecodeFileStrict' genesisFilePath

  let tx1 = mkTx txBody1
      toMempoolState :: NewEpochState CurrentEra -> MempoolState CurrentEra
      toMempoolState NewEpochState {nesEs = EpochState {esLState}} =
        (_utxoState esLState, _delegationState esLState)
      pp :: PParams CurrentEra
      pp =
        def
      -- { _minfeeA = 20,
      --   _minfeeB = 30,
      --   _costmdls = Map.singleton PlutusV1 (CostModel $ 0 <$ fromJust defaultCostModelParams),
      --   _maxValSize = 1000,
      --   _maxTxSize = fromIntegral (maxBound :: Int),
      --   _maxTxExUnits = maxTxExUnits,
      --   _collateralPercentage = collateralPercentage,
      --   _maxCollateralInputs = fromIntegral (maxCollateralInputs :: Word16)
      -- }
      !globals = mkGlobals genesis pp
      !slotNo = SlotNo 55733343
  -- let fp = "/home/lehins/iohk/chain/mainnet/latest-ledger-state.bin"
  -- fp <-
  --   execParser $
  --     info
  --       ( optsParser
  --           <* abortOption
  --             (ShowHelpText Nothing)
  --             (long "help" <> short 'h' <> help "Display this message.")
  --       )
  --       ( header
  --           "ledger-state:performance - Benchmark for ledger state performance"
  --       )
  -- runMode (Run defaultConfig) Prefix []
  -- defaultMain
  --   [ env (readNewEpochState fp <* putStrLn ("Read NewEpochState: " ++ show fp)) $ \newEpochState ->
  --       let utxo = getUTxO newEpochState
  --           (_, minTxOut) = Map.findMin $ unUTxO utxo
  --           (_, maxTxOut) = Map.findMax $ unUTxO utxo
  --           setAddr =
  --             Set.fromList [getTxOutAddr minTxOut, getTxOutAddr maxTxOut]
  --        in bgroup "getFilteredUTxO" $
  --             [ env (pure setAddr) $
  --                 bench "MinMaxTxId" . nf (getFilteredUTxO newEpochState)
  --             ]
  --   ]
  es <- readNewEpochState ledgerStateFilePath
  putStrLn ("Read NewEpochState: " ++ show ledgerStateFilePath)
  defaultMain
    [ env (pure es) $ \newEpochState ->
        bgroup
          "reapplyTx"
          [ env (pure tx1) $
              bench "Tx1"
                . nf
                  ( either (error . show) id
                      . reapplyTx globals (mkMempoolEnv newEpochState slotNo) (toMempoolState newEpochState)
                  )
          ],
      env (pure es) $ \newEpochState ->
        let utxo = getUTxO newEpochState
            (_, minTxOut) = Map.findMin $ unUTxO utxo
            (_, maxTxOut) = Map.findMax $ unUTxO utxo
            setAddr =
              Set.fromList [getTxOutAddr minTxOut, getTxOutAddr maxTxOut]
         in bgroup "MinMaxTxId" $
              [ env (pure setAddr) $
                  bench "getFilteredNewUTxO" . nf (getFilteredNewUTxO utxo),
                env (pure setAddr) $
                  bench "getFilteredOldUTxO" . nf (getFilteredOldUTxO utxo)
              ]
    ]

getUTxO :: NewEpochState era -> UTxO era
getUTxO = _utxo . _utxoState . esLState . nesEs

getFilteredNewUTxO ::
  Era era =>
  UTxO era ->
  Set (Addr (Crypto era)) ->
  UTxO era
getFilteredNewUTxO (UTxO fullUTxO) addrSet =
  UTxO $ Map.filter checkAddr fullUTxO
  where
    compactAddrSet = Set.map compactAddr addrSet
    checkAddr out =
      case getTxOutEitherAddr out of
        Left addr -> addr `Set.member` addrSet
        Right cAddr -> cAddr `Set.member` compactAddrSet

getFilteredOldUTxO ::
  Era era =>
  UTxO era ->
  Set (Addr (Crypto era)) ->
  UTxO era
getFilteredOldUTxO (UTxO fullUTxO) addrs =
  UTxO $
    Map.filter (\out -> getTxOutCompactAddr out `Set.member` addrSBSs) fullUTxO
  where
    addrSBSs = Set.map compactAddr addrs
