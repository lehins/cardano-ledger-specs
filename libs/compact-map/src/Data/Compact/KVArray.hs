{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Compact.KVArray where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Primitive
import qualified Data.IntMap.Strict as IntMap
import Data.Kind
import qualified Data.Map.Strict as Map
import Data.Massiv.Array as A
import Data.Massiv.Array.Mutable.Algorithms as A
import Data.Massiv.Array.Unsafe as A
import Data.Maybe

-- | Create `KVVector` from a `Map`
fromMap :: (Manifest vr v, Manifest kr k) => Map.Map k v -> Vector (KV kr vr) (KVPair k v)
fromMap m = fromAscListN (Map.size m) $ Map.toAscList m
{-# INLINE fromMap #-}

-- | Convert a sorted `KVVector` into `Map`
toMapSorted :: (Eq k, Manifest kr k, Manifest vr v) => Vector (KV kr vr) (KVPair k v) -> Map.Map k v
toMapSorted = Map.fromAscList . toAscListSorted
{-# INLINE toMapSorted #-}


-- | Create `KVVector` from an `IntMap`
fromIntMap ::
     (Manifest kr Int, Manifest vr v)
  => IntMap.IntMap v
  -> Vector (KV kr vr) (KVPair Int v)
fromIntMap m = fromAscListN (IntMap.size m) $ IntMap.toAscList m
{-# INLINE fromIntMap #-}

-- | Convert a sorted `KVVector` into `IntMap`
toIntMapSorted ::
     (Manifest kr Int, Manifest vr v)
  => Vector (KV kr vr) (KVPair Int v)
  -> IntMap.IntMap v
toIntMapSorted = IntMap.fromAscList . toAscListSorted
{-# INLINE toIntMapSorted #-}

fromAscList :: (Manifest vr v, Manifest kr k) => [(k, v)] -> Vector (KV kr vr) (KVPair k v)
fromAscList = A.fromList Seq . fmap (\(k, v) -> KVPair k v)
{-# INLINE fromAscList #-}

fromAscListN :: (Manifest vr v, Manifest kr k) => Int -> [(k, v)] -> Vector (KV kr vr) (KVPair k v)
fromAscListN n = A.compute . A.smap (\(k, v) -> KVPair k v) . A.sfromListN (Sz n)
{-# INLINE fromAscListN #-}

toAscListSorted :: (Manifest vr v, Manifest kr k) => Vector (KV kr vr) (KVPair k v) -> [(k, v)]
toAscListSorted = A.toList . A.map (\(KVPair k v) -> (k, v))
{-# INLINE toAscListSorted #-}

internKVVector ::
  (Manifest kr k, Ord k) =>
  k ->
  Vector (KV kr vr) (KVPair k v) ->
  k
internKVVector k = fromMaybe k . internKVVectorM k
{-# INLINE internKVVector #-}

internKVVectors ::
  (Manifest kr k, Ord k) =>
  k ->
  [Vector (KV kr vr) (KVPair k v)] ->
  k
internKVVectors k = fromMaybe k . listToMaybe . mapMaybe (internKVVectorM k)
{-# INLINE internKVVectors #-}

internKVVectorM ::
  (Manifest kr k, Ord k) =>
  k ->
  Vector (KV kr vr) (KVPair k v) ->
  Maybe k
internKVVectorM key (KVArray keys _values) =
  unsafeIndex keys <$> lookupIxSortedVector key keys
{-# INLINE internKVVectorM #-}

lookupSortedKVVector ::
  (Manifest kr k, Manifest vr v, Ord k) => k -> Vector (KV kr vr) (KVPair k v) -> Maybe v
lookupSortedKVVector key (KVArray keys values) = do
  i <- lookupIxSortedVector key keys
  indexM values i
{-# INLINE lookupSortedKVVector #-}

-- | Perform a binary search on a sorted vector
lookupIxSortedVector ::
  (Manifest kr k, Ord k) => k -> Vector kr k -> Maybe Ix1
lookupIxSortedVector key keys = go 0 (elemsCount keys)
  where
    go !l !u = do
      guard (l < u)
      let !i = ((u - l) `div` 2) + l
      key' <- indexM keys i
      case compare key key' of
        LT -> go l i
        GT -> go (i + 1) u
        EQ -> Just i
{-# INLINE lookupIxSortedVector #-}

sortKVMArray_ ::
  (Manifest kr k, Manifest kv v, Ord k, PrimBase m) =>
  Scheduler (PrimState m) () ->
  MVector (PrimState m) (KV kr kv) (KVPair k v) ->
  m ()
sortKVMArray_ = quicksortByM_ (\(KVPair k1 _) (KVPair k2 _) -> pure (compare k1 k2))
{-# INLINE sortKVMArray_ #-}

type KVVector kr vr k v = Vector (KV kr vr) (KVPair k v)

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
  {-# INLINE size #-}
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
