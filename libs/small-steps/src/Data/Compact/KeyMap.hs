{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}


module Data.Compact.KeyMap where

import Data.Foldable (foldl')
import Cardano.Prelude (HeapWords (..),Generic,runST,ST)
import Data.Word(Word64)
import qualified Data.Primitive.Array as PA
import Data.Bits
  ( Bits,(.&.),
    (.|.),
    complement,
    popCount,
    unsafeShiftL,
    unsafeShiftR,
    setBit,
    testBit,
    clearBit,
  )
import Data.Compact.Class
import GHC.Exts ((==#), reallyUnsafePtrEquality#, isTrue# )
import qualified Data.Map as Map
import Control.DeepSeq (NFData(..))
import qualified Data.Primitive.SmallArray as Small
import Data.Primitive.SmallArray()
import System.Random(RandomGen,genWord64,mkStdGen)
import Prettyprinter
import Data.Text(Text,pack)
import qualified Prettyprinter.Internal as Pretty
import Data.Set(Set)
import qualified Data.Set as Set
-- import Debug.Trace

-- type PArray = PA.Array
type PArray = Small.SmallArray

bin :: Integral n => n -> [n]
bin x = reverse (binary x)

-- ==========================================================================
-- bits, Segments, Paths. Breaking a Key into a sequence of small components

-- | Represent a set of small integers, they can range from 0 to 63
type Bitmap = Word64

-- | The number of bits in a segment. Can't be more than 6, because using Word64
--   as Bitmap can only accomodate 2^6 = 64 bits
bits :: Int  
bits = 6

-- | Ints in the range [0..63], represents 'bits' wide portion of a key
type Segment = Int

-- | Represents a list of 'bits', which when combined is in 1-1 correspondance with a Key
type Path = [Segment]

-- | The maximum value of a segment, as an Int
intSize :: Int     
intSize = 2 ^ bits

-- | The maximum value of a segment, as a Word64
wordSize :: Word64
wordSize = 2 ^ ((fromIntegral bits)::Word64)

-- | The length of a list of segments representing a key. Need to be carefull if a Key isn't evenly divisible by bits
pathSize :: Word64
pathSize = (if (mod 64 wbits)==0 then (div 64 wbits) else (div 64 wbits) + 1)
  where wbits = fromIntegral bits :: Word64

-- | Break up a Word64 into a Path
getpath :: Word64 -> Path
getpath w64 = loop pathSize w64 []
  where loop :: Word64 -> Word64 -> [Int] -> [Int]
        loop 0 _ ans = ans
        loop cnt n ans = loop (cnt - 1) (div n wordSize) ((fromIntegral (mod n wordSize)):ans)

-- ========================================================================
-- Keys 

data Key = Key  {-# UNPACK #-} !Word64
                {-# UNPACK #-} !Word64
                {-# UNPACK #-} !Word64
                {-# UNPACK #-} !Word64
  deriving (Eq,Ord,Show,NFData,Generic)

-- | The number of Word64 per key
wordsPerKey :: Int
wordsPerKey = 4

-- | The length of a Path for a Key (which might have multiple Word64's inside)
keyPathSize :: Int
keyPathSize = wordsPerKey * (fromIntegral pathSize)

genKey :: RandomGen b => b -> (Key, b)
genKey g = (Key w0 w1 w2 w3,g4)
  where (w0,g1) = genWord64 g
        (w1,g2) = genWord64 g1
        (w2,g3) = genWord64 g2
        (w3,g4) = genWord64 g3        

-- | Break up a Key into a Path
path :: Key -> Path
path (Key w0 w1 w2 w3) = getpath w0 ++ getpath w1 ++ getpath w2 ++ getpath w3

-- | A pair of a Key and its equivalent Path
data BitState = BitState Path !Key

-- Initialize a BitState from a Key
initBitState :: Key -> BitState
initBitState key = BitState (path key) key

-- | Obtain the Key from a BitState
getBytes :: BitState -> Key
getBytes (BitState _ bs) = bs

-- | Consume the next segment from a path 
nextBits :: String -> BitState -> (Int,BitState)
nextBits _ (BitState (x:xs) key) = (x,BitState xs key)
nextBits message (BitState [] key) =
   error ("NextBits out of bounds. The key "++show key++" has run out of bits."++message)
 
next2 :: BitState -> Key -> BitState
next2 (BitState ps _) key = (BitState (drop n (path key)) key)
   where n = (fromIntegral keyPathSize) - length ps 

showBM :: Bitmap -> String
showBM bm = show(bitmapToList bm)

bitmapToList :: Bits a => a -> [Int]
bitmapToList bm = loop 63 []
  where loop i ans | i < 0 = ans
        loop i ans = if testBit bm i then loop (i-1) (i:ans) else loop (i-1) ans

instance HeapWords Key where
  heapWords (Key _ _ _ _) = 5

instance Show BitState where
  show (BitState p key) = "(BitState "++show p++" "++show key++")"


-- ===============================================================

data HashMap v
    = Empty
    | Leaf {-# UNPACK #-} !Key v
    | One {-# UNPACK #-} !Int (HashMap v)                           -- 1 subtree
    | Two {-# UNPACK #-} !Bitmap (HashMap v) (HashMap v)            -- 2 subtrees
    | BitmapIndexed {-# UNPACK #-} !Bitmap                          -- 3 - (intSize - 1) subtrees
                    {-# UNPACK #-} !(Small.SmallArray (HashMap v))  
    | Full {-# UNPACK #-} !(Small.SmallArray (HashMap v))           -- intSize subtrees
  deriving (NFData,Generic)

heapAdd :: HeapWords a => a -> Int -> Int
heapAdd x ans = heapWords x + ans

heapPlus:: HeapWords a => Int -> a -> Int
heapPlus ans x = heapWords x + ans

instance HeapWords t => HeapWords (PA.Array t) where
   heapWords arr = foldl' heapPlus (2 + isize arr) arr

instance HeapWords v => HeapWords (HashMap v) where
  heapWords Empty = 1
  heapWords (One _ xs) = 3 + heapWords xs
  heapWords (Leaf _ v) = 6 + heapWords v  -- Change when Key changes
  heapWords (BitmapIndexed _ arr) = foldl' heapPlus 2 arr
  heapWords (Full arr) = foldl' heapPlus 1 arr
  heapWords (Two _ a b) = 4 + heapWords a + heapWords b                       

-- ======================================================================
-- Insertion

insert' :: BitState -> v -> HashMap v -> HashMap v
insert' bs0 v0 m0 = go bs0 v0 m0
  where
    go !state !x Empty = Leaf (getBytes state) x
    go !state x (One j node) =
        case (nextBits "One" state) of
          (i,state1) ->
            case compare j i of
              EQ -> One j (go state1 x node)
              LT -> Two (setBit (setBit 0 i) j) node (go state1 x Empty)
              GT -> Two (setBit (setBit 0 i) j) (go state1 x Empty) node
    go state x t@(Leaf bs1 y)
      | getBytes state == bs1 = if x `ptrEq` y then t else (Leaf bs1 x)
      | otherwise =  makeTwo state t (next2 state bs1) x
    go state x t@(BitmapIndexed bmap arr)
        | not(testBit bmap tagbits) =
            let !arr' = insertAt arr i $! (Leaf (getBytes state) x)
            in bitmapIndexedOrFull (bmap .|. m) arr'
        | otherwise = 
            let !st = index arr i
                !st' = go state1 x st
            in if st' `ptrEq` st
                  then t
                  else BitmapIndexed bmap (update arr i st') 
       where (!tagbits,!state1) = nextBits "BitmapIndexed" state
             m = setBit 0 tagbits
             i = sparseIndex bmap m
    go state x t@(Two bmap x0 x1)
        | not(testBit bmap tagbits) =
            let !arr' = insertAt (fromlist [x0,x1]) i $! (Leaf (getBytes state) x)
            in bitmapIndexedOrFull (bmap .|. m) arr'
        | otherwise =
            let !st = if i==0 then x0 else x1 -- index arr i
                !st' = go state1 x st
            in if st' `ptrEq` st
                  then t
                  else if i==0
                          then Two bmap st' x1
                          else Two bmap x0 st'
       where (!tagbits,!state1) = nextBits "Two" state
             m = setBit 0 tagbits
             i = sparseIndex bmap m
    go state x t@(Full arr) =
        let !st = index arr i
            !st' = go state1 x st
        in if st' `ptrEq` st
              then t
              else Full (update arr i st') 
       where (!tagbits,!state1) = nextBits "Full" state
             m = setBit 0 tagbits
             i = sparseIndex fullNodeMask m

makeTwo :: BitState -> HashMap v -> BitState -> v -> HashMap v
makeTwo state1 leaf1 state2 val2 
      | i1==i2 = One i1 (makeTwo state1' leaf1 state2' val2)
      | otherwise = -- trace ("MAKETWO (i1,i2)="++show(i1,i2)++"\n  state1="++show state1++"\n  state2="++show state2) $
                    if i1 < i2
                       then Two (setBits [i1,i2]) (Leaf (getBytes state1') val2) leaf1
                       else Two (setBits [i1,i2]) leaf1 (Leaf (getBytes state1') val2) 
   where (i1,state1') = nextBits "makeTwo1" state1
         (i2,state2') = nextBits ("makeTwo2 "++"\n  "++show state1++"\n  "++show state2) state2

insert :: Key -> v -> HashMap v -> HashMap v
insert bs v hashmap = insert' (initBitState bs) v hashmap            

fromList :: [(Key,v)] -> HashMap v
fromList ps = foldl' accum Empty ps
  where accum ans (k,v) = insert k v ans

-- =================================================================
-- Deletion

-- | Delete the Key encoded in the BitState from the HashMap
delete' :: BitState -> HashMap v -> HashMap v
delete' (BitState [] _) hm = hm -- Removing a bogus key, leaves 'hm' unchanged
delete' _ Empty = Empty
delete' (BitState _ k) (hm@(Leaf k2 _)) = if k==k2 then Empty else hm
delete' (BitState(i:is) k) (hm@(One j x)) = if i==j then oneE j (delete' (BitState is k) x) else hm
delete' (BitState(i:is) k) (hm@(Two bmap x y)) =
   if testBit bmap i
      then twoE bmap (delete' (BitState is k) x) (delete' (BitState is k) y)
      else hm
delete' (BitState(i:is) k) (hm@(BitmapIndexed bmap arr)) =
   if testBit bmap i
      then let m = setBit 0 i
               j = sparseIndex bmap m
               result = delete' (BitState is k) (index arr j)
           -- Consume an upwards floating Empty by removing that element from the array
           in case result of
               Empty -> bitmapE (clearBit bmap i) (remove arr j)
               _ -> BitmapIndexed bmap (update arr j result)
      else hm
delete' (BitState(i:is) k) (Full arr) =
   let m = setBit 0 i
       j = sparseIndex fullNodeMask m
       result = delete' (BitState is k) (index arr j)
   -- Consume an upwards floating Empty by removing that element from the array
   in case result of
        Empty -> BitmapIndexed (clearBit fullNodeMask i) (remove arr j)
        _ -> Full(update arr j result)

delete :: Key -> HashMap v -> HashMap v
delete k hm = delete' (initBitState k) hm

-- One of the invariants is that no Empty ever appears in any of the other
-- constructors of KeyMap.  So we make "smart" constructors that remove Empty
-- if it ever occurrs. This is necessary since 'delete' can turn a subtree
-- into Empty. The strategy is to float 'Empty' up the tree, until it can be
-- 'remove'd from one of the constructors with Array like components (One, Two, BitmapInded, Full).

-- Float Empty up over One
oneE :: Int -> HashMap v -> HashMap v
oneE _ Empty = Empty
oneE i x = One i x

-- Float Empty's up over Two
twoE :: Bitmap -> HashMap v -> HashMap v -> HashMap v
twoE _ Empty Empty = Empty
twoE bmap x Empty = oneE (ith bmap 0) x
twoE bmap Empty x = oneE (ith bmap 1) x
twoE bmap x y = Two bmap x y

-- | Get the 'ith' element from a Bitmap
ith :: Bitmap -> Int -> Int
ith bmap i = (bitmapToList bmap !! i)


-- | The first (smallest) Segment in a BitMap
firstSeg :: Bitmap -> Segment
firstSeg bmap = head(bitmapToList bmap)

-- Float Empty's up over BitmpIndexed, Note that if the size of the arr
-- becomes 2, then rebuild with Two rather than BitmapIndexed
bitmapE :: Bitmap -> PArray (HashMap v) -> HashMap v
bitmapE bmap arr | isize arr == 2 = twoE bmap (index arr 0) (index arr 1)
bitmapE bmap arr = bitmapIndexedOrFull bmap arr

-- ================================================================
-- aggregation in ascending order of keys

foldWithKey :: (ans -> Key -> v -> ans) -> ans -> HashMap v -> ans  
foldWithKey _ ans Empty = ans
foldWithKey accum ans (Leaf k v) = accum ans k v
foldWithKey accum ans (One _ x) = foldWithKey accum ans x
foldWithKey accum ans (Two _ x y) = foldWithKey accum (foldWithKey accum ans x) y
foldWithKey accum ans0 (BitmapIndexed _ arr) = loop ans0 0
  where n = isize arr
        loop ans i | i >= n = ans
        loop ans i = loop (foldWithKey accum ans (index arr i)) (i+1)
foldWithKey accum ans0 (Full arr) = loop ans0 0
  where n = isize arr
        loop ans i | i >= n = ans
        loop ans i = loop (foldWithKey accum ans (index arr i)) (i+1)        

-- ==================================================================
-- Lookup a key 

indexFromStateAndBitmap :: BitState -> Bitmap -> (BitState,Int)
indexFromStateAndBitmap state bmap = (state1,sparseIndex bmap m)
   where  (!bs,!state1) = nextBits "indexFromStateAndBitmap" state
          m = setBit 0 bs


lookup' :: BitState -> HashMap v -> Maybe v
lookup' _ Empty = Nothing
lookup' state (Leaf bs v) = if (getBytes state)==bs then Just v else Nothing
lookup' state (One i x) = if i==j then lookup' state' x else Nothing
  where (j,state') = nextBits "ONE in lookup" state
lookup' state (Two bm x0 x1) = if i==0 then lookup' state' x0 else lookup' state' x1
  where (state',i) = indexFromStateAndBitmap state bm
lookup' state (BitmapIndexed bm arr) = lookup' state' (index arr i)
  where (state',i) = indexFromStateAndBitmap state bm
lookup' state (Full arr) = lookup' state' (index arr i)
  where (state',i) = indexFromStateAndBitmap state fullNodeMask    

lookupHM :: Key -> HashMap v -> Maybe v
lookupHM bytes mp = lookup' (initBitState bytes) mp


-- ==========================================================
-- Split a HashMap into 3 parts

-- | return (smaller than 'key', has key?, greater than 'key')
splitHashMap:: BitState -> HashMap v -> (HashMap v,Maybe v,HashMap v)
splitHashMap (BitState [] _) hm = (hm,Nothing,Empty)
splitHashMap (BitState (i:is) key) hm =
  case splitBySegment i hm of
    (less,x,greater) ->
      case x of
        Empty -> (build less,Nothing,build greater)
        (Leaf k v) -> (build less,if key==k then (Just v) else Nothing,build greater)
        other ->  (reconstruct i less less1,ans,reconstruct i greater greater1)
          where (less1,ans,greater1) = splitHashMap (BitState is key) other

splitBySegment :: Segment -> HashMap v -> ([(Segment,HashMap v)],HashMap v, [(Segment,HashMap v)])
splitBySegment i _x | i < 0 = ([],Empty,[])
splitBySegment i _x | i > intSize =  ([],Empty,[])
splitBySegment _ Empty = ([],Empty,[])
splitBySegment _ (x@(Leaf _ _)) = ([],x,[])
splitBySegment i (x@(One j y)) =
   case compare i j of
     LT -> ([],Empty,[(i,x)])
     EQ -> ([],y,[])
     GT -> ([(i,x)],Empty,[])
splitBySegment i (Two bmap l h) = splitArrAtSeg i bmap (fromlist [l,h])
splitBySegment i (BitmapIndexed bmap arr) = splitArrAtSeg i bmap arr
splitBySegment i (Full arr) = splitArrAtSeg i fullNodeMask arr 

-- | Split an PArray at a particular Segment.
splitArrAtSeg:: Segment -> Bitmap -> PArray (HashMap v) -> ([(Int, HashMap v)], HashMap v, [(Int, HashMap v)])
splitArrAtSeg i bmap arr = (takeWhile smaller ps, match, dropWhile tooSmall ps)
    where ps = zip (bitmapToList bmap) (tolist arr)
          smaller (j,_) = j < i
          tooSmall (j,_) = j <= i
          same (j,_) = i==j
          match = case filter same ps of
            [] -> Empty
            ((_,x):_) -> x

-- | reconstruct a HashMap from list of previous Segments, and a single HashMap from the next Segment 
reconstruct :: Segment -> [(Segment, HashMap v)] -> HashMap v -> HashMap v
reconstruct _ xs Empty = build xs
reconstruct seg xs x = build (insertAscending (seg,x) xs)

-- | insert a Segment pair in ascending order of Segments, Keep it sorted.
insertAscending:: (Segment, HashMap v) -> [(Segment, HashMap v)] -> [(Segment, HashMap v)]
insertAscending (i,x) [] = [(i,x)]
insertAscending (i,x) (ws@((y@(j,_)):ys)) =
  case compare i j of
    LT -> (i,x):ws
    GT -> y : insertAscending (i,x) ys
    EQ -> (i,x):ys -- We know that the Segement i should never appear in the list

-- | Build a HashMap out of a list of Segment pairs.
build :: [(Segment, HashMap v)] -> HashMap v
build [] = Empty
build [(_,x)] = x
build [(j,x),(k,y)] = Two (setBits [j,k]) x y
build ps = bitmapIndexedOrFull (setBits (map fst ps)) (fromlist (map snd ps))


testSplit2 :: Int -> IO ()
testSplit2 i = putStrLn (unlines [show hm, " ",show pathx," ",show a, " ",show b, " ",show c])
  where keys = makeKeys 99 1000
        ps = zip (take 12 keys) [0..]
        hm :: HashMap Int
        hm = fromList ps
        state@(BitState pathx _) = (initBitState (keys !! i))
        (a,b,c) = splitHashMap state hm


-- ===========================================================
-- Maximum and Minimum Key

-- | Get the smallest key, NOT the smallest value
getMin :: HashMap v -> Maybe (Key,v)
getMin Empty = Nothing
getMin (Leaf k v) = Just (k,v)
getMin (One _ x) = getMin x
getMin (Two _ x _) = getMin x
getMin (BitmapIndexed _ arr) = getMin (index arr 0)
getMin (Full arr) = getMin (index arr 0)

-- | Get the largest key, NOT the largest value
getMax :: HashMap v -> Maybe (Key,v)
getMax Empty = Nothing
getMax (Leaf k v) = Just (k,v)
getMax (One _ x) = getMax x
getMax (Two _ _ y) = getMax y
getMax (BitmapIndexed _ arr) = getMax (index arr (isize arr - 1))
getMax (Full arr) = getMax (index arr (isize arr - 1))

-- ==================================================

-- | The (key,value) pairs (subset) of 'h1' where key is in the domain of both 'h1' and 'h2'
intersect :: HashMap v -> HashMap v -> HashMap v
intersect map1 map2 =
   case next map1 map2 of
     Nothing -> Empty
     Just k -> leapfrog k map1 map2 Empty

leapfrog :: Key -> HashMap v -> HashMap v -> HashMap v -> HashMap v
leapfrog k x y ans =
  case (lub k x,lub k y) of
       (Just(k1,v1,h1),Just(k2,_,h2)) ->
          case next h1 h2 of
            Just k3 -> leapfrog k3 h1 h2 (if k1==k2 then insert k1 v1 ans else ans)
            Nothing -> (if k1==k2 then insert k1 v1 ans else ans)
       _ -> ans

-- | Find the smallest key <= 'key', and a HashMap of everything bigger than 'key'
lub :: Key -> HashMap v -> Maybe (Key, v, HashMap v)
lub key hm =
  case splitHashMap (initBitState key) hm of
    (_,Just _,Empty) -> Nothing
    (_,Just v,hm2) -> Just(key,v,hm2)
    (_,Nothing,hm1) ->
       case getMin hm1 of
         Just (k,v) -> Just(k,v,hm1) 
         Nothing -> Nothing

next :: HashMap v1 -> HashMap v2 -> Maybe Key
next x y = case (getMin x,getMin y) of
            (Just (k1,_),Just (k2,_)) -> Just(max k1 k2)
            _ -> Nothing

testIntersect :: HashMap Int
testIntersect = intersect h1x h2x

h1x, h2x :: HashMap Int
h1x = fromList [pairs !! 3,pairs !! 5, pairs !! 11, pairs !! 6, pairs !! 4]
h2x = fromList [pairs !! 3,pairs !! 7, pairs !! 4, pairs !! 6, pairs !! 8] 
-- =========================================================

-- | Domain restrict 'hkm' to those Keys found in 's'. This algorithm
--   assumes the set 's' is small compared to 'hm'.
domainRestrict :: HashMap v -> Set Key -> HashMap v
domainRestrict hm s = Set.foldl' accum Empty s
  where accum ans key =
          case lookupHM key hm of
            Nothing -> ans
            Just v -> insert key v ans

hmdr :: HashMap Int
hmdr = fromList (take 10 pairs)

set:: Set Key
set = Set.fromList [ bpairs !! 3, bpairs !! 8, bpairs !! 20]

-- ==========================================
-- Operations on Bits and Bitmaps

-- | Check if two the two arguments are the same value.  N.B. This
-- function might give false negatives (due to GC moving objects.)
ptrEq :: a -> a -> Bool
ptrEq x y = isTrue# (reallyUnsafePtrEquality# x y ==# 1#)
{-# INLINE ptrEq #-}

bitsPerSubkey :: Int
bitsPerSubkey = 4
{-# INLINE bitsPerSubkey #-}

maxChildren :: Int
maxChildren = 1 `unsafeShiftL` bitsPerSubkey
{-# INLINE maxChildren #-}

subkeyMask :: Bitmap
subkeyMask = 1 `unsafeShiftL` bitsPerSubkey - 1
{-# INLINE subkeyMask #-}

sparseIndex :: Bitmap -> Bitmap -> Int
sparseIndex b m = popCount (b .&. (m - 1))
{-# INLINE sparseIndex #-}

-- | Create a 'BitmapIndexed' or 'Full' node.
bitmapIndexedOrFull :: Bitmap -> PArray (HashMap v) -> HashMap v
bitmapIndexedOrFull b ary
    | b == fullNodeMask = Full ary
    | otherwise         = BitmapIndexed b ary
{-# INLINE bitmapIndexedOrFull #-}

-- | A bitmask with the 'bitsPerSubkey' least significant bits set.
fullNodeMask :: Bitmap
fullNodeMask = complement (complement 0 `unsafeShiftL` maxChildren)
{-# INLINE fullNodeMask #-}

-- | A Bitmap represents a set. Split it into 3 parts (set1,present,set2)
--   where 'set1' is all elements in 'bm' less than 'i'
--         'present' is if 'i' is in the set 'bm'
--         'set2' is all elements in 'bm' greater than 'i'
splitBitmap :: Bitmap -> Int -> (Bitmap,Bool,Bitmap)
splitBitmap bm 0 = (0,testBit bm 0, clearBit bm 0)
splitBitmap bm i = (unsafeShiftR (unsafeShiftL bm (64-i)) (64-i)
                   ,testBit bm i
                   ,unsafeShiftL (unsafeShiftR bm (i+1)) (i+1))

setBits :: [Int] -> Bitmap
setBits xs = foldl' setBit 0 xs 


-- =======================================================================
-- Operations to make new arrays out off old ones with small changes

-- | /O(n)/ Make a copy of an Array that removes the 'i'th element. Decreasing the size by 1.
remove :: ArrayPair arr marr a => arr a -> Int -> arr a
remove arr i = fst(withMutArray n action)
   where n = (isize arr) - 1
         action marr = do
            mcopy marr 0 arr 0 i
            mcopy marr i arr (i+1) (n-i)


-- | /O(n)/ Overwrite the element at the given position in this array,
update :: PArray t -> Int -> t -> PArray t
update arr i _t
  | i<0 || i >= (isize arr)
  = error ("index out of bounds in update "++show i++" not in range (0,"++show (isize arr -1)++")")
update arr i t = fst(withMutArray size1 action)
  where size1 = isize arr
        action marr = do
          mcopy marr 0 arr 0 i
          mwrite marr i t         
          mcopy marr (i+1) arr (i+1) (size1 - (i+1))


-- | /O(n)/ Insert an element at the given position in this array,
-- increasing its size by one.
insertM :: PArray e -> Int -> e -> ST s (PArray e)
insertM ary idx b
   | idx < 0 || idx > counter = error ("Bounds check in insertAt "++show idx++" not in range 0.."++show (counter))
   | otherwise = do
           mary <- mnew (counter+1)
           mcopy mary 0 ary 0 idx
           mwrite mary idx b
           mcopy mary (idx+1) ary idx (counter-idx)
           mfreeze mary
  where !counter = isize ary
{-# INLINE insertM #-}

-- | /O(n)/ Insert an element at the given position in this array,
-- increasing its size by one.
insertAt :: PArray e -> Int -> e -> PArray e
insertAt arr idx b = runST(insertM arr idx b)
{-# INLINE insertAt #-}

-- | Create a new Array of size 'n' filled with objects 'a'
arrayOf :: Int -> a -> PArray a
arrayOf n a =  runST $ do
  marr <- mnew n
  let loop i
       | i < n = mwrite marr i a >> loop (i+1)
       | otherwise = pure ()
  loop 0
  arr <- mfreeze marr
  pure arr
{-# INLINE arrayOf #-}

-- | Extract a slice from an array
subarray :: ArrayPair arr2 marr a => Int -> Int -> arr2 a -> arr2 a
subarray 0 hi arr | hi == (isize arr -1) = arr
subarray lo hi arr = fst(withMutArray size action)
  where size = max (hi - lo + 1) 0
        action marr = mcopy marr 0 arr lo size
{-# INLINE subarray #-}


-- | Split an array into 2 partial copies, where a1 appears at the last index 
--   of the first copy and 'a2' appears at the 0th index of the second.
--   splitArrAt (fromlist [0,1,2,3,4,5,6]) 4 44 45
--   (fromlist [0,1,2,3,44],fromlist [45,5,6])
splitArrayAt :: PArray a -> Int -> a -> a -> (PArray a, PArray a)
splitArrayAt arr i a1 a2 = project (with2MutArray size1 size2 action)
  where project (arr1,arr2, _state) = (arr1,arr2)
        size1 = i + 1
        size2 = isize arr - i
        action marr1 marr2 = do
           mcopy marr1 0 arr 0 i
           mwrite marr1 i a1
           mwrite marr2 0 a2
           mcopy marr2 1 arr (i+1) (size2 - 1)

-- =========================================================================

makeKeys :: Int -> Int -> [Key]
makeKeys seed cnt = loop (mkStdGen seed) cnt []
  where loop _g i ans | i <= 0 = ans
        loop g i ans = case genKey g of
                         (key,g2) -> loop g2 (i-1) (key : ans)

testt :: Int -> IO ()
testt n = do
   let (hmap,output) = tests n
       histArr = histo hmap
   -- hbytes <- recursiveSize $!! hmap
   -- putStrLn ("hbytes = "++show hbytes++"\n"++output)
   putStrLn output
   putStrLn ("histogram "++show(tolist histArr))


tests :: Int -> (HashMap Int, String)
tests n = (hashmap,unlines
      [ "bits per level = "++show bits
      , "num levels = "++show keyPathSize
      , "empty = "++show empty
      , "leaf  = "++show leaf
      , "one   = "++show one
      , "two   = "++show two
      , "bits  = "++show bit     
      , "full  = "++show full
      , "hwords = "++show hwords
      , "mwords = "++show mwords
      , "diff   = "++show(hwords - mwords)++" %"++show((hwords*100) `div` mwords)
      , "depth = "++show (hdepth hashmap)
      ])
  where hashmap = fromList (take n pairs)
        mapmap = Map.fromList (take n pairs)
        (empty,one,two,leaf,bit,full) = count hashmap
        hwords = heapWords hashmap
        mwords = heapWords mapmap

count :: HashMap v -> (Int,Int,Int,Stat Int,Stat Int,Int)
count x = go 0 x (0,0,0,mempty,mempty,0)
  where go _ Empty (e,o,t,l,b,f) = (e+1,o,t,l,b,f)
        go d (One _ y) (e,o,t,l,b,f) = go (1 + d) y (e,1+o,t,l,b,f)
        go d (Two _ z y) (e,o,t,l,b,f) = go (1+d) y (go (1+d) z (e,o,1+t,l,b,f))
        go d (Leaf _ _) (e,o,t,l,b,f) = (e,o,t,add d l,b,f)
        go d (BitmapIndexed _ arr) (e,o,t,l,b,f) =
          foldr (go (length arr + d)) (e,o,t,l,add (length arr) b,f) arr
        go d (Full arr) (e,o,t,l,b,f) = foldr  (go (length arr + d)) (e,o,t,l,b,f+1) arr



hdepth :: HashMap v -> Int
hdepth Empty = 0
hdepth (One _ x) = 1 + hdepth x
hdepth (Leaf _ _) = 1
hdepth (BitmapIndexed _ arr) = 1+ maximum(foldr (\ x ans -> hdepth x : ans) [] arr)            
hdepth (Full arr) = 1+ maximum(foldr (\ x ans -> hdepth x : ans) [] arr)
hdepth (Two _ x y) = 1 + max (hdepth x) (hdepth y)

increment :: (ArrayPair arr marr a, Num a) => marr s a -> Int -> ST s ()
increment marr i = do { n <- mindex marr i; mwrite marr i (n+1) }

histogram :: HashMap v -> PA.MutableArray s Int -> ST s ()
histogram Empty _ = pure ()
histogram (One _ x) marr = increment marr 1  >> histogram x marr
histogram (Leaf _ _) _ = pure ()
histogram (BitmapIndexed _ arr) marr = increment marr (isize arr-1) >> mapM_ (\ x -> histogram x marr) arr     
histogram (Full arr) marr = increment marr (intSize-1) >> mapM_ (\ x -> histogram x marr) arr  
histogram (Two _ x y) marr = increment marr 2 >> histogram x marr >> histogram y marr

histo :: HashMap v -> PA.Array Int
histo x = fst(withMutArray intSize process)
  where process marr = do { initialize (intSize - 1) ; histogram x marr }
           where initialize n | n <0 = pure ()
                 initialize n = mwrite marr n 0 >> initialize (n-1)



bpairs :: [Key]
bpairs = makeKeys 99 1500000
         -- makeKeys 3 15

pairs :: [ (Key,Int) ]
pairs = zip bpairs [0..] 

-- ===================================================

data Stat n = Stat n n (Maybe n) (Maybe n)

liftM:: (t -> t -> t) -> Maybe t -> Maybe t -> Maybe t
liftM f (Just x) (Just y) = Just(f x y)
liftM _ Nothing (Just y) = Just y
liftM _ (Just x) Nothing = Just x
liftM _ Nothing Nothing = Nothing

instance (Ord n,Num n) => Semigroup (Stat n) where
 (Stat c1 s1 mx1 mn1) <> (Stat c2 s2 mx2 mn2) =
    Stat (c1+c2) (s1 + s2) (liftM max mx1 mx2) (liftM min mn1 mn2)

instance (Ord n,Num n) => Monoid (Stat n) where
  mempty = Stat 0 0 Nothing Nothing

instance (Integral n,Show n) => Show (Stat n) where
  show (Stat c s mx mn) = "{count= "++show c++", sum="++show s++", max="++show mx++
                            ", min="++show mn++
                            (if c==0 then "}" else ", avg="++show(div s c)++"}")

add :: (Num n,Ord n) => n -> Stat n -> Stat n
add n stat = (Stat 1 n (Just n) (Just n)) <> stat

-- ====================
-- Debugging functions

bug :: Int -> IO (HashMap Int)
bug n = do
   let ps = take n pairs -- zip (makeKeys 3 n) [0..]
       hh (k@(Key m0 m1 _ _ ),v) = show m0++" "++show m1++" "++show (path k)++" "++show v
   putStrLn (unlines (map hh ps))

   -- putStrLn (show (fromList ps))
   pure (fromList ps)

try :: [(Key,Int)] -> IO ()
try ps = do
   let  hh (k@(Key m0 m1 _ _),v) = show m0++" "++show m1++" "++show (path k)++" "++show v
   putStrLn (unlines (map hh ps))
   putStrLn (show (fromList ps))


testlookup :: Int -> Int -> Bool
testlookup seed n = all ok results
  where ps = zip (makeKeys seed n) [0..]
        keymap :: HashMap Int
        keymap = fromList ps
        results = [ (i,lookupHM (fst(ps !! i)) keymap) | i <- [0..(n-1)]]
        ok (_,Just _) = True
        ok (i,Nothing) = error ("testlookup failure: "++show i++"   "++show pair++"\n"++
                                show (path (fst pair))++"\n  "++show keymap)
          where pair = (ps !! i)

-- ======================================================================================
-- Helper functions for Pretty Printers 

newtype PrettyAnn = Width Int

type Ann = [PrettyAnn]

type PDoc = Doc Ann

class PrettyA t where
  prettyA :: t -> PDoc

instance PrettyA Int where
  prettyA = ppInt

instance PrettyA Word64 where
  prettyA = ppWord64
  
ppWord64 :: Word64 -> Doc a
ppWord64 = viaShow

ppInt :: Int -> Doc a
ppInt = viaShow

text :: Text -> Doc ann
text = pretty

isEmpty :: Doc ann -> Bool
isEmpty Pretty.Empty = True
isEmpty _ = False

-- | ppSexp x [w,y,z] --> (x w y z)
ppSexp :: Text -> [PDoc] -> PDoc
ppSexp con = ppSexp' (text con)

ppSexp' :: PDoc -> [PDoc] -> PDoc
ppSexp' con fields =
  group $
    flatAlt
      (hang 2 (encloseSep lparen rparen space docs))
      (encloseSep lparen rparen space docs)
  where
    docs = if isEmpty con then fields else con : fields

-- | Vertical layout with commas aligned on the left hand side
puncLeft :: Doc ann -> [Doc ann] -> Doc ann -> Doc ann -> Doc ann
puncLeft open [] _ close = hsep [open, close]
puncLeft open [x] _ close = hsep [open, x, close]
puncLeft open (x : xs) coma close = align (sep ((open <+> x) : help xs))
  where
    help [] = mempty
    help [y] = [hsep [coma, y, close]]
    help (y : ys) = (coma <+> y) : help ys

ppList :: (x -> Doc ann) -> [x] -> Doc ann
ppList p xs =
  group $
    flatAlt
      (puncLeft lbracket (map p xs) comma rbracket)
      (encloseSep (lbracket <> space) (space <> rbracket) (comma <> space) (map p xs))

-- | x == y
equate :: Doc a -> Doc a -> Doc a
equate x y = group (flatAlt (hang 2 (sep [x <+> text "=", y])) (hsep [x, text "=", y]))

ppArray :: (Indexable arr a) => (a -> PDoc) -> arr a -> PDoc
ppArray f arr = ppList f (tolist arr)

-- ====================================
-- Pretty Printer for HashMap

ppKey :: Key -> PDoc
ppKey (Key w0 _ _ _) = ppWord64 w0

ppBitmap :: Word64 -> PDoc
ppBitmap x = text (pack(showBM x))

ppHashMap :: (v -> PDoc) -> HashMap v -> PDoc
ppHashMap p (Leaf k v) = ppSexp "L" [ppKey k,p v]
ppHashMap _ Empty = text "E"
ppHashMap p (One x mp) = ppSexp "O" [ppInt x,ppHashMap p mp]
ppHashMap p (Two x m1 m2) = ppSexp "T" [ppBitmap x ,ppHashMap p m1, ppHashMap p m2]
ppHashMap p (BitmapIndexed x arr) = ppSexp "B" [ppList q (zip (bitmapToList x) (tolist arr))]
  where q (i,a) = ppInt i <+> ppHashMap p a
ppHashMap p (Full arr) = ppSexp "F" [ppList q (zip (bitmapToList fullNodeMask) (tolist arr))]
  where q (i,a) = ppInt i <+> ppHashMap p a

instance PrettyA v => Show (HashMap v) where
   show x = show(ppHashMap prettyA x)
   showList xs x = unlines (map (\ y -> "\n"++ show(ppHashMap prettyA y)) xs) ++ x

