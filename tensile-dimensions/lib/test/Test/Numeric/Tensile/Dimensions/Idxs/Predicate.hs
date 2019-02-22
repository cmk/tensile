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

--TODO add arbitrary permutations
--pred_permute_idxs :: forall ds. Dims ds -> Bool

{- TODO is this sketchy behavior?
> foo d (majorToMinor d)
[7,5,3,1,6,4,2,0]
> foo d (transposeIdxs majorToMinor d)
[7,3,6,2,5,1,4,0]
> foo d (minorToMajor d)
[7,6,5,4,3,2,1,0]
> foo d (transposeIdxs minorToMajor d)
[7,6,5,4,3,2,1,0]
-}
