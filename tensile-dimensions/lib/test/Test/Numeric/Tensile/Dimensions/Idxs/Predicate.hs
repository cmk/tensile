module Test.Numeric.Tensile.Dimensions.Idxs.Predicate where

import Numeric.Tensile.Dimensions

pred_max_diffIdx :: forall (d :: [Nat]). Dims d -> Bool
pred_max_diffIdx d = (fromIntegral $ 1 + diffIdxs d (maxBound' d) (minBound' d)) == (product . fromDims $ d)


{-
 -
-- test -


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

reifySomeDims (reverse [2,3,3]) $ \p -> overDimIdx_ (reflect p) print
> reifySomeDims (reverse [2,3,3]) $ \p -> overDimIdx_ (reflect p) print
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

> reifySomeDims (reverse [2,3,3]) $ \p -> Numeric.Tensile.Operations.Linear.Internal.overDimIdx_ (reflect p) print
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

> overDimIdx_ (dims @'[2,3,3]) print
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

> overDimIdx_ (dims @'[2,3,3]) (\i -> remapIdxs re (dims @'[2,3,3]) i print)
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

overDimIdx_ f print
overDimIdx_ f (\i -> remapIdxs re f i (\_ j -> print j))



-}
