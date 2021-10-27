{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Ledger.State.Massiv where

import Data.Maybe
import Control.Monad.Primitive
import Cardano.Ledger.Coin
import Cardano.Ledger.Credential
import Cardano.Ledger.Keys as Keys
import Cardano.Ledger.State.UTxO
import Cardano.Ledger.Shelley.TxBody hiding (TxId, TxIn)
import Control.DeepSeq
import Control.Monad
import qualified Data.IntMap.Strict as IntMap
import Data.Kind
import qualified Data.Map.Strict as Map
import Data.Massiv.Array as A
import Data.Massiv.Array.Mutable.Algorithms as A
import Data.Massiv.Array.Unsafe as A
import Foreign.Storable
import Foreign.Ptr
import Data.Word

type KVVector kr vr k v = Vector (KV kr vr) (KVPair k v)

data SnapShotM crypto = SnapShotM
  { ssStake :: !(KVVector B S (Credential 'Staking crypto) Coin),
    ssDelegations :: !(KVVector B B (Credential 'Staking crypto) (KeyHash 'StakePool crypto)),
    ssPoolParams :: !(KVVector B B (KeyHash 'StakePool crypto) (PoolParams crypto))
  }

instance NFData (SnapShotM C) where
  rnf (SnapShotM s d p) = s `deepseq` d `deepseq` rnf p

data SnapShotsM crypto =
  SnapShotsM
    { ssPstakeMark :: !(SnapShotM crypto)
    , ssPstakeSet :: !(SnapShotM crypto)
    , ssPstakeGo :: !(SnapShotM crypto)
    , ssFeeSS :: !Coin
    }
instance NFData (SnapShotsM C) where
  rnf (SnapShotsM r s g f) = r `deepseq` s `deepseq` g `deepseq` rnf f

--deriving (Show, Eq)

instance Storable Coin where
  sizeOf _ = sizeOf (undefined :: Word64)
  {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined :: Word64)
  {-# INLINE alignment #-}
  poke ptr (Coin c) =
    case integerToWord64 c of
      Nothing -> error $ "Impossible Coin overflow: " ++ show c
      Just w -> poke (castPtr ptr) w
  {-# INLINE poke #-}
  peek ptr = Coin . fromIntegral <$> peek (castPtr ptr :: Foreign.Ptr.Ptr Word64)
  {-# INLINE peek #-}

internKVArray ::
     (Manifest kr k, Ord k)
  => k
  -> Vector (KV kr vr) (KVPair k v)
  -> k
internKVArray k = fromMaybe k . internKVArrayM k
{-# INLINE internKVArray #-}

internKVArrays ::
     (Manifest kr k, Ord k)
  => k
  -> [Vector (KV kr vr) (KVPair k v)]
  -> k
internKVArrays k = fromMaybe k . listToMaybe . mapMaybe (internKVArrayM k)
{-# INLINE internKVArrays #-}


internKVArrayM ::
     (Manifest kr k, Ord k)
  => k
  -> Vector (KV kr vr) (KVPair k v)
  -> Maybe k
internKVArrayM key (KVArray keys _values) = do
  i <- lookupIxSortedArray key keys
  indexM keys i
{-# INLINE internKVArrayM #-}


quicksortKVMArray_ ::
  (Manifest kr k, Manifest kv v, Ord k, PrimBase m) =>
  Scheduler (PrimState m) () ->
  MVector (PrimState m) (KV kr kv) (KVPair k v) ->
  m ()
quicksortKVMArray_ = quicksortByM_ (\(KVPair k1 _) (KVPair k2 _) -> pure (compare k1 k2))
{-# INLINE quicksortKVMArray_ #-}


fromMap :: (Manifest vr v, Manifest kr k) => Map.Map k v -> Vector (KV kr vr) (KVPair k v)
fromMap m = fromAscListN (Map.size m) $ Map.toAscList m

fromIntMap :: Manifest vr v => IntMap.IntMap v -> Vector (KV P vr) (KVPair Int v)
fromIntMap m = fromAscListN (IntMap.size m) $ IntMap.toAscList m

toIntMap :: Manifest vr v => Vector (KV P vr) (KVPair Int v) -> IntMap.IntMap v
toIntMap = IntMap.fromAscList . toAscList

fromAscList :: (Manifest vr v, Manifest kr k) => [(k, v)] -> Vector (KV kr vr) (KVPair k v)
fromAscList = A.compute . A.smap (\(k, v) -> KVPair k v) . A.sfromList

fromAscListN :: (Manifest vr v, Manifest kr k) => Int -> [(k, v)] -> Vector (KV kr vr) (KVPair k v)
fromAscListN n = A.compute . A.smap (\(k, v) -> KVPair k v) . A.sfromListN (Sz n)

toAscList :: (Manifest vr v, Manifest kr k) => Vector (KV kr vr) (KVPair k v) -> [(k, v)]
toAscList = A.toList . A.map (\(KVPair k v) -> (k, v))

lookupIxSortedArray ::
  (Manifest kr k, Ord k) => k -> Vector kr k -> Maybe Ix1
lookupIxSortedArray key keys = go 0 (elemsCount keys)
  where
    go !l !u = do
      guard (l < u)
      let !i = ((u - l) `div` 2) + l
      key' <- indexM keys i
      case compare key key' of
        LT -> go l i
        GT -> go (i + 1) u
        EQ -> Just i
{-# INLINE lookupIxSortedArray #-}

lookupSortedKVArray ::
  (Manifest kr k, Manifest vr v, Ord k) => k -> Vector (KV kr vr) (KVPair k v) -> Maybe v
lookupSortedKVArray key (KVArray keys values) = do
  i <- lookupIxSortedArray key keys
  indexM values i
{-# INLINE lookupSortedKVArray #-}

data KV kr vr = KV !kr !vr

data KVPair k v = KVPair !k !v

type family KVValue e :: Type

type family KVKey e :: Type

type instance KVKey (KVPair k v) = k

type instance KVValue (KVPair k v) = v

data instance Array (KV kr vr) ix e = KVArray
  { keysArray :: !(Array kr ix (KVKey e)),
    valsArray :: !(Array vr ix (KVValue e))
  }

instance
  (NFData (Array kr ix k), (NFData (Array kv ix v))) =>
  NFData (Array (KV kr kv) ix (KVPair k v))
  where
  rnf KVArray {..} = keysArray `deepseq` valsArray `deepseq` ()

instance (Size kr, Size vr) => Size (KV kr vr) where
  size (KVArray k _) = size k
  unsafeResize sz (KVArray k v) = KVArray (unsafeResize sz k) (unsafeResize sz v)

instance (Strategy kr, Strategy vr) => Strategy (KV kr vr) where
  setComp c (KVArray k v) = KVArray (setComp c k) (setComp c v)
  getComp (KVArray k v) = getComp k <> getComp v

instance (Source kr k, Source vr v) => Source (KV kr vr) (KVPair k v) where
  unsafeLinearIndex (KVArray keys vals) ix =
    KVPair (unsafeLinearIndex keys ix) (unsafeLinearIndex vals ix)
  {-# INLINE unsafeLinearIndex #-}
  unsafeLinearSlice ix sz (KVArray keys vals) =
    KVArray (unsafeLinearSlice ix sz keys) (unsafeLinearSlice ix sz vals)
  {-# INLINE unsafeLinearSlice #-}

data instance MArray s (KV kr vr) ix e = KVMArray
  { keysMArray :: !(MArray s kr ix (KVKey e)),
    valsMArray :: !(MArray s vr ix (KVValue e))
  }

instance (Manifest kr k, Manifest vr v) => Manifest (KV kr vr) (KVPair k v) where
  unsafeLinearIndexM (KVArray keys vals) ix =
    KVPair (unsafeLinearIndexM keys ix) (unsafeLinearIndexM vals ix)
  {-# INLINE unsafeLinearIndexM #-}

  sizeOfMArray (KVMArray k _) = sizeOfMArray k
  {-# INLINE sizeOfMArray #-}

  unsafeResizeMArray sz (KVMArray k v) =
    KVMArray (unsafeResizeMArray sz k) (unsafeResizeMArray sz v)
  {-# INLINE unsafeResizeMArray #-}

  unsafeLinearSliceMArray ix sz (KVMArray keys vals) =
    KVMArray (unsafeLinearSliceMArray ix sz keys) (unsafeLinearSliceMArray ix sz vals)
  {-# INLINE unsafeLinearSliceMArray #-}

  unsafeThaw (KVArray k v) = KVMArray <$> unsafeThaw k <*> unsafeThaw v
  {-# INLINE unsafeThaw #-}

  unsafeFreeze comp (KVMArray k v) =
    KVArray <$> unsafeFreeze comp k <*> unsafeFreeze comp v
  {-# INLINE unsafeFreeze #-}

  unsafeNew sz = KVMArray <$> unsafeNew sz <*> unsafeNew sz
  {-# INLINE unsafeNew #-}

  unsafeLinearRead (KVMArray keys vals) ix =
    KVPair <$> unsafeLinearRead keys ix <*> unsafeLinearRead vals ix
  {-# INLINE unsafeLinearRead #-}

  unsafeLinearWrite (KVMArray keys vals) ix (KVPair k v) = do
    unsafeLinearWrite keys ix k
    unsafeLinearWrite vals ix v
  {-# INLINE unsafeLinearWrite #-}

  initialize (KVMArray keys vals) = initialize keys >> initialize vals
  {-# INLINE initialize #-}

  unsafeLinearSet (KVMArray keys vals) offset len (KVPair k v) = do
    unsafeLinearSet keys offset len k
    unsafeLinearSet vals offset len v
  {-# INLINE unsafeLinearSet #-}

  unsafeLinearCopy (KVMArray keysFrom valsFrom) iFrom (KVMArray keysTo valsTo) iTo n = do
    unsafeLinearCopy keysFrom iFrom keysTo iTo n
    unsafeLinearCopy valsFrom iFrom valsTo iTo n
  {-# INLINE unsafeLinearCopy #-}

  unsafeArrayLinearCopy (KVArray keysFrom valsFrom) iFrom (KVMArray keysTo valsTo) iTo n = do
    unsafeArrayLinearCopy keysFrom iFrom keysTo iTo n
    unsafeArrayLinearCopy valsFrom iFrom valsTo iTo n
  {-# INLINE unsafeArrayLinearCopy #-}

  unsafeLinearShrink (KVMArray keys vals) sz =
    KVMArray <$> unsafeLinearShrink keys sz <*> unsafeLinearShrink vals sz
  {-# INLINE unsafeLinearShrink #-}

  unsafeLinearGrow (KVMArray keys vals) sz =
    KVMArray <$> unsafeLinearGrow keys sz <*> unsafeLinearGrow vals sz
  {-# INLINE unsafeLinearGrow #-}
