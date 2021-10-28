{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Compact.KVVector where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Primitive
import qualified Data.IntMap.Strict as IntMap
import Data.Kind
import qualified Data.Map.Strict as Map
import Data.Vector.Generic as VG
import Data.Vector.Generic.Mutable as VGM
import Data.Maybe as Maybe
import Data.Vector.Algorithms.Intro

-- -- | Create `KVVector` from a `Map`
-- fromMap :: (Manifest vr v, Manifest kr k) => Map.Map k v -> Vector (KV kr vr) (KVPair k v)
-- fromMap m = fromAscListN (Map.size m) $ Map.toAscList m
-- {-# INLINE fromMap #-}

-- | Convert a sorted `KVVector` into `Map`

toMapSorted ::
     (Eq k, VG.Vector kv k, VG.Vector vv v)
  => KVVector kv vv (KVPair k v)
  -> Map.Map k v
toMapSorted = Map.fromAscList . toAscListSorted
{-# INLINE toMapSorted #-}


-- -- | Create `KVVector` from an `IntMap`
-- fromIntMap ::
--      (Manifest kr Int, Manifest vr v)
--   => IntMap.IntMap v
--   -> Vector (KV kr vr) (KVPair Int v)
-- fromIntMap m = fromAscListN (IntMap.size m) $ IntMap.toAscList m
-- {-# INLINE fromIntMap #-}

-- | Convert a sorted `KVVector` into `IntMap`
toIntMapSorted ::
     (VG.Vector kv Int, VG.Vector vv v)
  => KVVector kv vv (KVPair Int v)
  -> IntMap.IntMap v
toIntMapSorted = IntMap.fromAscList . toAscListSorted
{-# INLINE toIntMapSorted #-}

-- fromAscList :: (Manifest vr v, Manifest kr k) => [(k, v)] -> Vector (KV kr vr) (KVPair k v)
-- fromAscList = A.fromList Seq . fmap (\(k, v) -> KVPair k v)
-- {-# INLINE fromAscList #-}

-- fromAscListN :: (Manifest vr v, Manifest kr k) => Int -> [(k, v)] -> Vector (KV kr vr) (KVPair k v)
-- fromAscListN n = A.compute . A.smap (\(k, v) -> KVPair k v) . A.sfromListN (Sz n)
-- {-# INLINE fromAscListN #-}

toAscListSorted ::
     (VG.Vector kv k, VG.Vector vv v)
  => KVVector kv vv (KVPair k v)
  -> [(k, v)]
toAscListSorted = VG.toList . VG.map (\(KVPair k v) -> (k, v))
{-# INLINE toAscListSorted #-}

internKVVector ::
     (Vector kv (KVKey a), Ord (KVKey a))
  => KVKey a
  -> KVVector kv vv a
  -> KVKey a
internKVVector k = fromMaybe k . internKVVectorM k
{-# INLINE internKVVector #-}

internKVVectors ::
     (Vector kv (KVKey a), Ord (KVKey a))
  => KVKey a
  -> [KVVector kv vv a]
  -> KVKey a
internKVVectors k = fromMaybe k . listToMaybe . Maybe.mapMaybe (internKVVectorM k)
{-# INLINE internKVVectors #-}

internKVVectorM ::
     (Vector kv (KVKey a), Ord (KVKey a))
  => KVKey a
  -> KVVector kv vv a
  -> Maybe (KVKey a)
internKVVectorM key (KVVector keys _values) =
  unsafeIndex keys <$> lookupIxSortedVector key keys
{-# INLINE internKVVectorM #-}

lookupSortedKVVector ::
     (Ord (KVKey a), Vector kv (KVKey a), Vector v (KVValue a))
  => KVKey a
  -> KVVector kv v a
  -> Maybe (KVValue a)
lookupSortedKVVector key (KVVector keys values) = do
  i <- lookupIxSortedVector key keys
  VG.indexM values i
{-# INLINE lookupSortedKVVector #-}

-- | Perform a binary search on a sorted vector
lookupIxSortedVector ::
  (VG.Vector kv k, Ord k) => k -> kv k -> Maybe Int
lookupIxSortedVector key keys = go 0 (VG.length keys)
  where
    go !l !u = do
      guard (l < u)
      let !i = ((u - l) `div` 2) + l
      case compare key (keys VG.! i) of
        LT -> go l i
        GT -> go (i + 1) u
        EQ -> Just i
{-# INLINE lookupIxSortedVector #-}

sortKVMArray_ ::
     (VGM.MVector kmv k, VGM.MVector vmv v, Ord k, PrimMonad m)
  => KVMVector kmv vmv (PrimState m) (KVPair k v)
  -> m ()
sortKVMArray_ = sortBy (\(KVPair k1 _) (KVPair k2 _) -> compare k1 k2)
{-# INLINE sortKVMArray_ #-}

class KV a where
  kvKey :: a -> KVKey a
  kvValue :: a -> KVValue a

data KVPair k v = KVPair !k !v

type family KVKey e :: Type

type family KVValue e :: Type

type instance KVKey (KVPair k v) = k

type instance KVValue (KVPair k v) = v

type instance KVKey (k, v) = k

type instance KVValue (k, v) = v

data KVVector kv vv a =
  KVVector
    { keysVector :: !(kv (KVKey a))
    , valsVector :: !(vv (KVValue a))
    }

data KVMVector kmv vmv s a = KVMVector
  { keysMVector :: !(kmv s (KVKey a)),
    valsMVector :: !(vmv s (KVValue a))
  }

type instance VG.Mutable (KVVector kv vv) = KVMVector (VG.Mutable kv) (VG.Mutable vv)


instance (NFData (kv k), (NFData (vv v))) => NFData (KVVector kv vv (k, v)) where
  rnf KVVector {..} = keysVector `deepseq` valsVector `deepseq` ()


instance (Vector kv k, Vector vv v) => Vector (KVVector kv vv) (k, v) where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (KVMVector kmv vmv) =
    KVVector <$> basicUnsafeFreeze kmv <*> basicUnsafeFreeze vmv

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (KVVector kv vv) = KVMVector <$> basicUnsafeThaw kv <*> basicUnsafeThaw vv

  {-# INLINE basicLength #-}
  basicLength (KVVector kv _) = VG.basicLength kv

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i n (KVVector kv vv) =
    KVVector (VG.basicUnsafeSlice i n kv) (VG.basicUnsafeSlice i n vv)

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (KVVector kv vv) i =
    (,) <$> basicUnsafeIndexM kv i <*> basicUnsafeIndexM vv i

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (KVMVector kvDst vvDst) (KVVector kvSrc vvSrc) =
    VG.basicUnsafeCopy kvDst kvSrc >> VG.basicUnsafeCopy vvDst vvSrc

instance (MVector kmv k, MVector vmv v) => MVector (KVMVector kmv vmv) (k, v) where
  {-# INLINE basicLength #-}
  basicLength (KVMVector kmv _) = VGM.basicLength kmv

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m (KVMVector kmv vmv) =
    KVMVector (VGM.basicUnsafeSlice j m kmv) (VGM.basicUnsafeSlice j m vmv)

  {-# INLINE basicOverlaps #-}
  basicOverlaps (KVMVector kmv1 vmv1) (KVMVector kmv2 vmv2) =
    VGM.basicOverlaps kmv1 kmv2 && VGM.basicOverlaps vmv1 vmv2

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n = do
    kmv1 <- basicUnsafeNew n
    vmv1 <- basicUnsafeNew n
    return (KVMVector kmv1 vmv1)

  {-# INLINE basicInitialize #-}
  basicInitialize (KVMVector kmv vmv) = basicInitialize kmv >> basicInitialize vmv

  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeReplicate n (k, v) =
    KVMVector <$> basicUnsafeReplicate n k <*> basicUnsafeReplicate n v

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (KVMVector kmv vmv) i =
    (,) <$> basicUnsafeRead kmv i <*> basicUnsafeRead vmv i

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (KVMVector kmv vmv) i (k, v) =
    basicUnsafeWrite kmv i k >> basicUnsafeWrite vmv i v

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (KVMVector kmvDst vmvDst) (KVMVector kmvSrc vmvSrc) =
    VGM.basicUnsafeCopy kmvDst kmvSrc >> VGM.basicUnsafeCopy vmvDst vmvSrc

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (KVMVector kmvDst vmvDst) (KVMVector kmvSrc vmvSrc) =
    basicUnsafeMove kmvDst kmvSrc >> basicUnsafeMove vmvDst vmvSrc

  {-# INLINE basicClear #-}
  basicClear (KVMVector kmv vmv) = basicClear kmv >> basicClear vmv




instance (NFData (kv k), (NFData (vv v))) => NFData (KVVector kv vv (KVPair k v)) where
  rnf KVVector {..} = keysVector `deepseq` valsVector `deepseq` ()


instance (Vector kv k, Vector vv v) => Vector (KVVector kv vv) (KVPair k v) where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (KVMVector kmv vmv) =
    KVVector <$> basicUnsafeFreeze kmv <*> basicUnsafeFreeze vmv

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (KVVector kv vv) = KVMVector <$> basicUnsafeThaw kv <*> basicUnsafeThaw vv

  {-# INLINE basicLength #-}
  basicLength (KVVector kv _) = VG.basicLength kv

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i n (KVVector kv vv) =
    KVVector (VG.basicUnsafeSlice i n kv) (VG.basicUnsafeSlice i n vv)

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (KVVector kv vv) i =
    KVPair <$> basicUnsafeIndexM kv i <*> basicUnsafeIndexM vv i

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (KVMVector kvDst vvDst) (KVVector kvSrc vvSrc) =
    VG.basicUnsafeCopy kvDst kvSrc >> VG.basicUnsafeCopy vvDst vvSrc

instance (MVector kmv k, MVector vmv v) => MVector (KVMVector kmv vmv) (KVPair k v) where
  {-# INLINE basicLength #-}
  basicLength (KVMVector kmv _) = VGM.basicLength kmv

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j m (KVMVector kmv vmv) =
    KVMVector (VGM.basicUnsafeSlice j m kmv) (VGM.basicUnsafeSlice j m vmv)

  {-# INLINE basicOverlaps #-}
  basicOverlaps (KVMVector kmv1 vmv1) (KVMVector kmv2 vmv2) =
    VGM.basicOverlaps kmv1 kmv2 && VGM.basicOverlaps vmv1 vmv2

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n = do
    kmv1 <- basicUnsafeNew n
    vmv1 <- basicUnsafeNew n
    return (KVMVector kmv1 vmv1)

  {-# INLINE basicInitialize #-}
  basicInitialize (KVMVector kmv vmv) = basicInitialize kmv >> basicInitialize vmv

  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeReplicate n (KVPair k v) =
    KVMVector <$> basicUnsafeReplicate n k <*> basicUnsafeReplicate n v

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (KVMVector kmv vmv) i =
    KVPair <$> basicUnsafeRead kmv i <*> basicUnsafeRead vmv i

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (KVMVector kmv vmv) i (KVPair k v) =
    basicUnsafeWrite kmv i k >> basicUnsafeWrite vmv i v

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (KVMVector kmvDst vmvDst) (KVMVector kmvSrc vmvSrc) =
    VGM.basicUnsafeCopy kmvDst kmvSrc >> VGM.basicUnsafeCopy vmvDst vmvSrc

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (KVMVector kmvDst vmvDst) (KVMVector kmvSrc vmvSrc) =
    basicUnsafeMove kmvDst kmvSrc >> basicUnsafeMove vmvDst vmvSrc

  {-# INLINE basicClear #-}
  basicClear (KVMVector kmv vmv) = basicClear kmv >> basicClear vmv

