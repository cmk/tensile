module Test.Numeric.Tensile.Dimensions.Idxs.Predicate where

import Numeric.Tensile.Dimensions
import qualified Numeric.Tensile.Dimensions.Types as T

pred_max_diff_idxs :: forall ds. Dims ds -> Bool
pred_max_diff_idxs ds = 
  (fromIntegral $ 1 + diffIdxs ds (maxBound' ds) (minBound' ds)) == (product . fromDims $ ds)

pred_sum_idxs :: forall ds. Dims ds -> Bool
pred_sum_idxs ds = foldIdxs ds (\_ a -> a + 1) 0 == (product . fromDims $ ds)

pred_transpose_idxs :: forall ds. Dims ds -> Bool
pred_transpose_idxs ds = check ds minorToMajor == check (T.unsafeReverse ds) (transposeIdxs minorToMajor)
  where check :: Dims ds -> (forall ds i. Integral i => Dims ds -> Idxs ds -> i) -> [Word]
        check ds f = foldIdxs ds (\i xs -> f ds i : xs) []



{-
 -
-- test -

> foldIdxs (dims @'[]) (\_ a -> a + 1) 0
1
pred_remap_idxs :: forall ds. Dims ds -> Bool
pred_remap_idxs ds = (traceShowId $ foldIdxs ds (normal ds) []) == (traceShowId $ foldIdxs ds (adjusted ds) [])
  where normal ds is xs = minorToMajor ds is : xs
        adjusted ds is xs = remapIdxsTest majorToMinor ds is : xs

foldIdxs :: Dims ds -> (Idxs ds -> a -> a) -> a -> a
foldIdxs U k = k U
foldIdxs (Snoc ds d) k = foldIdxs ds k'
  where k' is = go 0
          where go i | i >= fromDim d = id
                     | otherwise = go (i + 1) . k (is `snoc` Idx i)

unsafeReifyDims [2,4] $ \p -> foldIdxs (reflect p) (\i xs -> majorToMinor (reflect p) i : xs) []
unsafeReifyDims [2,4] $ \p -> foldIdxs (reflect p) (\i xs -> majorToMinor (reflect p) i : xs) []


unsafeReifyDims [2,4] $ \p -> forMIdxs_ (reflect p) (print . majorToMinor (reflect p))



pred_diffIdx d = reifySomeDims d f
  where f = (fromIntegral $ 1 + diffIdx d (maxBound' d) (minBound' d)) == (product . fromDims $ d)


> reifyDims [2,2,3] $ \p -> (fromIntegral $ 1 + diffIdx (reflect p) maxBound minBound) == (product . fromDims $ reflect p)

diffIdx :: Dims xs -> Idxs xs -> Idxs xs -> Int
diffIdx d i j = _diffIdx (unsafeReverse d) (unsafeReverse i) (unsafeReverse j)

--test 
a :: Maybe (Idxs '[2, 3, 3])
a = idxsFromWord [2, 1, 1]  

b :: Maybe (Idxs '[2, 3, 3])
b = idxsFromWord [2, 3, 3] 

check :: Dims d -> Idxs d -> Idxs d -> Perm (Rank d) -> (Perm (Rank d) -> Idxs d -> Idxs d) -> [(Int, Int)]
check d i j p f = foldDimPartIdx i j acc []
  where 
    acc i l = if (i/=f p i) then (1 + minorToMajor d i, 1 + minorToMajor d (f p i)) : l else l

res = liftM2 (\i j -> check d233 i j ttt $ filterIdx triangular'') a b

res == Just [(11,13),(12,16),(15,17)]


--test
--reify :: a -> (forall s. Reifies s a => Proxy s -> r) -> r

reify d233 $ \p -> totalDim' $ reflect p

reifySomeDims (reverse [2,3,3]) $ \p -> forMIdxs_ (reflect p) print
> reifySomeDims (reverse [2,3,3]) $ \p -> forMIdxs_ (reflect p) print
Idxs [1,1,1]
Idxs [2,1,1]
Idxs [3,1,1]
Idxs [1,2,1]
Idxs [2,2,1]
Idxs [3,2,1]
Idxs [1,3,1]
Idxs [2,3,1]
Idxs [3,3,1]
Idxs [1,1,2]
Idxs [2,1,2]
Idxs [3,1,2]
Idxs [1,2,2]
Idxs [2,2,2]
Idxs [3,2,2]
Idxs [1,3,2]
Idxs [2,3,2]
Idxs [3,3,2]

> reifySomeDims (reverse [2,3,3]) $ \p -> Numeric.Tensile.Operations.Linear.Internal.forMIdxs_ (reflect p) print
Idxs [1,1,1]
Idxs [1,1,2]
Idxs [1,2,1]
Idxs [1,2,2]
Idxs [1,3,1]
Idxs [1,3,2]
Idxs [2,1,1]
Idxs [2,1,2]
Idxs [2,2,1]
Idxs [2,2,2]
Idxs [2,3,1]
Idxs [2,3,2]
Idxs [3,1,1]
Idxs [3,1,2]
Idxs [3,2,1]
Idxs [3,2,2]
Idxs [3,3,1]
Idxs [3,3,2]

-- test -

re :: Perm (Rank '[2, 3, 3])
re = reversal

a :: Idxs '[2, 3, 3]
a = fromJust $ idxsFromWords [2, 1, 3]  

> forMIdxs_ (dims @'[2,3,3]) print
Idxs [1,1,1]
Idxs [2,1,1]
Idxs [1,2,1]
Idxs [2,2,1]
Idxs [1,3,1]
Idxs [2,3,1]
Idxs [1,1,2]
Idxs [2,1,2]
Idxs [1,2,2]
Idxs [2,2,2]
Idxs [1,3,2]
Idxs [2,3,2]
Idxs [1,1,3]
Idxs [2,1,3]
Idxs [1,2,3]
Idxs [2,2,3]
Idxs [1,3,3]
Idxs [2,3,3]

> forMIdxs_ (dims @'[2,3,3]) (\i -> remapIdxs re (dims @'[2,3,3]) i print)
Idxs [1,1,1]
Idxs [2,1,1]
Idxs [3,1,1]
Idxs [1,2,1]
Idxs [2,2,1]
Idxs [3,2,1]
Idxs [1,3,1]
Idxs [2,3,1]
Idxs [3,3,1]
Idxs [1,1,2]
Idxs [2,1,2]
Idxs [3,1,2]
Idxs [1,2,2]
Idxs [2,2,2]
Idxs [3,2,2]
Idxs [1,3,2]
Idxs [2,3,2]
Idxs [3,3,2]

forMIdxs_ f print
forMIdxs_ f (\i -> remapIdxs re f i (\_ j -> print j))



-}
