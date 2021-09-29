
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Cardano.Ledger.State.Massiv where

import Foreign.Storable
import GHC.IO
import qualified GHC.Exts
import Cardano.Ledger.State.UTxO
import Control.Monad
import Cardano.Ledger.Compactible
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.ByteString.Short as BSS
import qualified Cardano.Address.Style.Byron as AB
import qualified Cardano.Address.Style.Icarus as AI
import qualified Cardano.Address.Style.Shelley as AS
import qualified Cardano.Chain.Common as Byron
import Cardano.Crypto.Hash.Class
import qualified Cardano.Crypto.Hashing as Byron
import Cardano.Ledger.Address
import Cardano.Ledger.Alonzo
import Cardano.Ledger.Alonzo.Tx as Alonzo
import Cardano.Ledger.Alonzo.TxBody as Alonzo
import Cardano.Ledger.BaseTypes
import Cardano.Ledger.Credential
import Cardano.Ledger.Crypto
import Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Hashes as Hashes
import qualified Cardano.Ledger.Keys as Keys
import qualified Cardano.Ledger.Mary.Value as Mary
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.API as Shelley
import Cardano.Ledger.Shelley.CompactAddr
import Cardano.Ledger.Shelley.TxBody
import Cardano.Ledger.Shelley.UTxO
import Conduit
import Data.Aeson as Aeson
import Data.Aeson.Parser as Aeson
import Data.Aeson.Types as Aeson
import Data.Attoparsec.ByteString as Atto
import qualified Data.Attoparsec.ByteString.Char8 as Atto8
import qualified Data.ByteString.Base16 as B16
import Data.Conduit.Attoparsec
import qualified Data.Conduit.List as C
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import GHC.StableName
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A
import Data.Massiv.Array.Mutable.Algorithms as A
import Control.DeepSeq

newtype UTxOv = UTxOv (Vector B (TxIn C, Alonzo.TxOut CurrentEra))

newtype UTxOv' = UTxOv' (Vector B (TxId C, IntMap.IntMap TxOut'))

loadMassivUTxO :: FilePath -> IO (ArrayMap (TxIn C) (Alonzo.TxOut CurrentEra))
loadMassivUTxO fp = do
  da <-
    consumeUTxO fp $
    foldlC (\a (!txIn, !txOut) -> A.cons (MapElt txIn txOut) a) A.empty
  vec <-
    withLoadMArray_ da $
    quicksortByM_
      (\(MapElt txIn1 _) (MapElt txIn2 _) -> pure (compare txIn1 txIn2))
  pure $ ArrayMap vec


-- loadMassivUTxO' :: FilePath -> IO (ArrayMap (TxIn C) TxOut')
-- loadMassivUTxO' fp = do
--   ma <- newMArray' (Sz1 3272356)
--   _ <- consumeUTxO fp $
--     foldMC (\i (!txIn, !txOut) -> (i + 1) <$ write_ ma i (MapElt txIn txOut)) 0
--   withMassivScheduler_ Par $ \ scheduler ->
--     quicksortByM_ (\(MapElt txIn1 _) (MapElt txIn2 _) ->
--                      pure (compare txIn1 txIn2)) scheduler ma
--   ArrayMap <$> A.unsafeFreeze Par ma

data CredRef
  = CredKeyRef !(Keys.KeyHash 'Shelley.Witness C)
  | CredScriptRef !(Shelley.ScriptHash C)
  deriving Eq

data StakeRef = StakeRef !CredRef
              | StakePtr !Ptr
              | StakeNull

data Addr'
  = AddrBoot' !(CompactAddr C)
  | Addr' !Network !CredRef !StakeRef

data TxOut'
  = TxOut' !Addr' !(CompactForm (Mary.Value C))
  | TxOutDH' !Addr' !(CompactForm (Mary.Value C)) !(DataHash C)


data UTxOs = UTxOs
  { utxoMap :: !(ArrayMap (TxId C) (IntMap.IntMap TxOut'))
  , utxoKeyHashSet :: !(RefSet (Keys.KeyHash 'Shelley.Witness C))
  , utxoScriptHashSet :: !(RefSet (Shelley.ScriptHash C))
  , utxoDataHashSet :: !(RefSet (DataHash C))
  }

loadMassivUTxOv' ::
     ArrayMap (TxIn C) TxOut' -> ArrayMap (TxId C) (IntMap.IntMap TxOut')
loadMassivUTxOv' (ArrayMap v) = do
  let
    collect (i, Nothing) = do
      e <- v !? i
      collect (i + 1, Just e)
    collect (i, Just (MapElt (TxIn txId txIx) txOut)) =
      collectForward i txId $! IntMap.singleton (fromIntegral txIx) txOut
    collectForward !i !txId !m =
      case v !? i of
        Nothing -> Just (MapElt txId m, (i, Nothing))
        Just e@(MapElt (TxIn txId' txIx') txOut')
          | txId' == txId ->
            collectForward (i + 1) txId $! IntMap.insert (fromIntegral txIx') txOut' m
          | otherwise -> Just (MapElt txId m, (i + 1, Just e))
  ArrayMap $ compute (sunfoldrN (size v) collect (0, Nothing))


loadMassivUTxOs :: FilePath -> IO UTxOs
loadMassivUTxOs fp = do
  -- let !bss = BSS.pack [1..28]
  --     m = Map.singleton bss "Some arbitrary value"
  --     i = Map.findIndex bss m
  --     !(!bss', _) = Map.elemAt i m
  -- ptr <- IO $ \s ->
  --   case GHC.Exts.anyToAddr# bss s of
  --     (# s', addr# #) -> (# s', GHC.Exts.Ptr addr# #)
  -- ptr' <- IO $ \s ->
  --   case GHC.Exts.anyToAddr# bss' s of
  --     (# s', addr# #) -> (# s', GHC.Exts.Ptr addr# #)
  -- sn <- makeStableName bss
  -- sn' <- makeStableName bss'
  -- putStrLn $ "Stable names equal: " <> show (eqStableName sn sn')
  -- putStrLn $ "Pointers equal: " <> show (ptr == (ptr' :: GHC.Exts.Ptr ()))
  -- -- BS.unsafeUseAsCString bss $ \ptr1 ->
  -- --   BS.unsafeUseAsCString bss' $ \ptr2 ->
  -- --     putStrLn $ "Underlying pointers are of course equal: " <> show (ptr1 == ptr2)
  -- putStrLn $ "But ShortByteString type pointers are not equal: " <> show ptr <> " /= " <> show ptr'
  -- undefined
  case hashFromBytes $ BS.pack [1..28] of
    Nothing -> error "Mistake converting hash bytes"
    Just !h -> do
      let !(Keys.KeyHash kh) = Keys.KeyHash h :: Keys.KeyHash 'Shelley.Witness StandardCrypto
          !m = Map.singleton kh "Some arbitrary value"
          !i = Map.findIndex kh m
          !(!kh', _) = Map.elemAt i m
      ptr <- IO $ \s ->
        case GHC.Exts.anyToAddr# kh s of
          (# s', addr# #) -> (# s', GHC.Exts.Ptr addr# #)
      ptr' <- IO $ \s ->
        case GHC.Exts.anyToAddr# kh' s of
          (# s', addr# #) -> (# s', GHC.Exts.Ptr addr# #)
      sn <- makeStableName kh
      sn' <- makeStableName kh'
      putStrLn $ "Stable names equal: " <> show (eqStableName sn sn')
      putStrLn $ "Pointers equal: " <> show (ptr == (ptr' :: GHC.Exts.Ptr ()))
      error $ "Pointers: " <> show ptr <> " /= " <> show ptr'

  -- ArrayMap vTxOut <- loadMassivUTxO fp
  -- putStrLn "Starting to fold"
  -- u <-
  --   case foldrS convertToRef (mempty, mempty, mempty, mempty) vTxOut of
  --     (!khMap, !shMap, !dhMap, vec) -> do
  --       putStrLn "Done to folding, starting to compute"
  --       arrMap <- computeIO vec
  --       putStrLn "Computed"
  --       pure $! UTxOs
  --         { utxoMap = loadMassivUTxOv' $ ArrayMap arrMap
  --         , utxoKeyHashSet = refSetFromMap khMap
  --         , utxoScriptHashSet = refSetFromMap shMap
  --         , utxoDataHashSet = refSetFromMap dhMap
  --         }
  -- putStrLn $ unlines
  --   [ "utxoMap: " <> show (A.size (unArrayMap (utxoMap u)))
  --   , "utxoKeyHashSet: " <> show (A.size (unRefSet (utxoKeyHashSet u)))
  --   , "utxoScriptHashSet: " <> show (A.size (unRefSet (utxoScriptHashSet u)))
  --   , "utxoDataHashSet: " <> show (A.size (unRefSet (utxoDataHashSet u)))
  --   ]
  -- verifyCreds (utxoKeyHashSet u) (utxoScriptHashSet u) (unArrayMap (utxoMap u))
  -- pure u
  -- where
  --   insertCredential cred khmap shmap =
  --     case cred of
  --       KeyHashObj kh
  --         | (khRef, khmap') <- insertRefMap (Keys.asWitness kh) khmap ->
  --           (CredKeyRef khRef, khmap', shmap)
  --       ScriptHashObj sh
  --         | (shRef, shmap') <- insertRefMap sh shmap ->
  --           (CredScriptRef shRef, khmap, shmap')
  --   convertToRef ::
  --        MapElt (TxIn C) (Alonzo.TxOut CurrentEra)
  --     -> ( Map.Map (Shelley.KeyHash 'Shelley.Witness C) Int
  --        , Map.Map (Shelley.ScriptHash C) Int
  --        , Map.Map (DataHash C) Int
  --        , Vector DL (MapElt (TxIn C) TxOut'))
  --     -> ( Map.Map (Shelley.KeyHash 'Shelley.Witness C) Int
  --        , Map.Map (Shelley.ScriptHash C) Int
  --        , Map.Map (DataHash C) Int
  --        , Vector DL (MapElt (TxIn C) TxOut'))
  --   convertToRef (MapElt txIn txOut) (!khMap, !shMap, !dhMap, vec) =
  --     let (!cAddr', !mkTxOut', !dhMap') =
  --           case txOut of
  --             Alonzo.TxOutCompact cAddr cVal ->
  --               (cAddr, \a -> TxOut' a cVal, dhMap)
  --             Alonzo.TxOutCompactDH cAddr cVal dh
  --               | (dhRef, dhmap) <- insertRefMap dh dhMap ->
  --                 (cAddr, \a -> TxOutDH' a cVal dhRef, dhmap)
  --         (!addr', !khMap', !shMap') =
  --           case decompactAddr cAddr' of
  --             AddrBootstrap _ -> (AddrBoot' cAddr', khMap, shMap)
  --             Addr ni pc sr ->
  --               let (!pcRef, !khmap, !shmap) = insertCredential pc khMap shMap
  --                   mkAddr' = Addr' ni pcRef
  --                in case sr of
  --                     StakeRefBase cred
  --                       | (!credRef, !khmap', !shmap') <- insertCredential cred khmap shmap ->
  --                         (mkAddr' (StakeRef credRef), khmap', shmap')
  --                     StakeRefPtr ptr -> (mkAddr' (StakePtr ptr), khmap, shmap)
  --                     StakeRefNull -> (mkAddr' StakeNull, khmap, shmap)
  --         !elt = MapElt txIn (mkTxOut' addr')
  --         vec' :: Vector DL (MapElt (TxIn C) TxOut')
  --         vec' = cons elt vec
  --      in (khMap', shMap', dhMap', vec')

verifyCreds ::
     (A.Source r (MapElt k (t TxOut')), Index a, Foldable t)
  => RefSet (Shelley.KeyHash 'Shelley.Witness C)
  -> RefSet (Shelley.ScriptHash C)
  -> A.Array r a (MapElt k (t TxOut'))
  -> IO ()
verifyCreds keys scripts txs =
  A.iforM_ txs $ \i (MapElt _ im) ->
    Control.Monad.forM_ im $ \case
      TxOut' addr' _ -> checkAddr' i addr'
      TxOutDH' addr' _ _ -> checkAddr' i addr'
  where
    checkAddr' i = \case
      AddrBoot' _ -> pure ()
      Addr' _ pc sr -> check i pc
    check i =
      \case
        CredKeyRef kr -> do
          krInTxOut <- makeStableName kr
          krInSet <- makeStableName $ lookupRefSet' kr keys
          unless (eqStableName krInTxOut krInSet) $
            error $ "TxOut at index: " <> show i <> " has not a shared key ref"
        CredScriptRef kr -> do
          krInTxOut <- makeStableName kr
          krInSet <- makeStableName $ lookupRefSet' kr scripts
          unless (eqStableName krInTxOut krInSet) $
            error $
            "TxOut at index: " <> show i <> " has not a shared script ref"

data MapElt k v =
  MapElt
    { mapEltKey :: !k
    , mapEltValue :: !v
    }

newtype ArrayMap k v =
  ArrayMap
    { unArrayMap :: Vector B (MapElt k v)
    }

lookupArrayMap :: Ord k => k -> ArrayMap k v -> Maybe v
lookupArrayMap k (ArrayMap vec) =
  mapEltValue <$> lookupSortedOn mapEltKey k vec

lookupArrayMap' :: (Show k, Ord k) => k -> ArrayMap k v -> v
lookupArrayMap' k a =
  case lookupArrayMap k a of
    Nothing -> error $ "Key should have been there: " ++ show k
    Just v -> v

data SetElt v =
  SetElt
    { setEltRef :: !v
    , setEltRefCounter :: !Int
    }

newtype RefSet v =
  RefSet
    { unRefSet :: Vector B (SetElt v)
    }


refSetFromMap :: Map.Map v Int -> RefSet v
refSetFromMap m =
  RefSet $
  compute $
  smap (uncurry SetElt) $ sfromListN (Sz1 (Map.size m)) $ Map.toAscList m

insertRefMap :: Ord v => v -> Map.Map v Int -> (v, Map.Map v Int)
insertRefMap v m =
  case Map.lookupIndex v m of
    Nothing -> (v, Map.insert v 1 m)
    Just i
      | (!v', _) <- Map.elemAt i m -> (v', Map.adjust (+ 1) v' m)

lookupRefSet :: Ord v => v -> RefSet v -> Maybe v
lookupRefSet k (RefSet vec) = setEltRef <$> lookupSortedOn setEltRef k vec


lookupRefSet' :: (Show v, Ord v) => v -> RefSet v -> v
lookupRefSet' k a =
  case lookupRefSet k a of
    Nothing -> error $ "Key should have been there: " ++ show k
    Just v -> v

lookupSortedOn :: (Manifest r a, Ord b) => (a -> b) -> b -> Vector r a -> Maybe a
lookupSortedOn f e v = go (k `div` 2) k
  where
    Sz k = size v
    go !i !n = do
      guard (i /= n)
      b <- indexM v i
      case compare e (f b) of
        LT -> go (i `div` 2) i
        GT -> go ((n - i) `div` 2) n
        EQ -> Just b


-- loadMassivUTxO'' :: FilePath -> IO UTxOv
-- loadMassivUTxO'' fp = do
--   ma <- newMArray' (Sz1 3272356)
--   _ <- consumeUTxO fp (foldMC (\i e@(!_, !_) -> (i + 1) <$ write_ ma i e) 0)
--   rebalanceM_ ma
--   UTxOv <$> A.unsafeFreeze Par ma


-- rebalanceM_ ma =
--   withMassivScheduler_ Par $ \ scheduler ->
--     quicksortByM'_ (\x y -> pure (compare (fst x) (fst y))) scheduler ma


-- quicksortCustomM_ ::
--      (Manifest r e, MonadPrimBase s m)
--   => (e -> e -> m Bool)
--   -> (e -> e -> m Bool)
--   -> Scheduler s ()
--   -> MVector s r e
--   -> m ()
-- quicksortCustomM_ fLT fEQ scheduler marr =
--   scheduleWork scheduler $ qsort (numWorkers scheduler) 0 (unSz (sizeOfMArray marr) - 1)
--   where
--     ltSwap i j = do
--       ei <- unsafeLinearRead marr i
--       ej <- unsafeLinearRead marr j
--       lt <- fLT ei ej
--       if lt
--         then do
--           unsafeLinearWrite marr i ej
--           unsafeLinearWrite marr j ei
--           pure ei
--         else pure ej
--     {-# INLINE ltSwap #-}
--     getPivot lo hi = do
--       let !mid = (hi + lo) `div` 2
--       _ <- ltSwap mid lo
--       _ <- ltSwap hi lo
--       ltSwap mid hi
--     {-# INLINE getPivot #-}
--     qsort !n !lo !hi =
--       when (lo < hi) $ do
--         p <- getPivot lo hi
--         l <- unsafeUnstablePartitionRegionM marr (`fLT` p) lo (hi - 1)
--         h <- unsafeUnstablePartitionRegionM marr (`fEQ` p) l hi
--         if n > 0
--           then do
--             let !n' = n - 1
--             scheduleWork scheduler $ qsort n' lo (l - 1)
--             scheduleWork scheduler $ qsort n' h hi
--           else do
--             qsort n lo (l - 1)
--             qsort n h hi
-- {-# INLINE quicksortInternalM_ #-}


-- unsafeUnstablePartitionRegionCustomM ::
--      forall r e m. (Manifest r e, PrimMonad m)
--   => MVector (PrimState m) r e
--   -> (e -> m Bool)
--   -> Ix1 -- ^ Start index of the region
--   -> Ix1 -- ^ End index of the region
--   -> m Ix1
-- unsafeUnstablePartitionRegionCustomM marr f start end = fromLeft start (end + 1)
--   where
--     fromLeft i j
--       | i == j = pure i
--       | otherwise = do
--         e <- f =<< unsafeLinearRead marr i
--         if e
--           then fromLeft (i + 1) j
--           else fromRight i (j - 1)
--     fromRight i j
--       | i == j = pure i
--       | otherwise = do
--         x <- unsafeLinearRead marr j
--         e <- f x
--         if e
--           then do
--             unsafeLinearWrite marr j =<< unsafeLinearRead marr i
--             unsafeLinearWrite marr i x
--             fromLeft (i + 1) j
--           else fromRight i (j - 1)
-- {-# INLINE unsafeUnstablePartitionRegionM #-}

