module Test.Numeric.Tensile.Dimensions.Idxs.Predicate where

import Numeric.Tensile.Dimensions
import qualified Numeric.Tensile.Dimensions.Types as T
import Unsafe.Coerce 

pred_max_diff_idxs :: forall ds. Dims ds -> Bool
pred_max_diff_idxs ds = 
  (fromIntegral $ 1 + diffIdxs ds (maxBound' ds) (minBound' ds)) == (product . fromDims $ ds)

pred_sum_idxs :: forall ds. Dims ds -> Bool
pred_sum_idxs ds = foldIdxs ds (\_ a -> a + 1) 0 == (product . fromDims $ ds)

check :: Dims ds -> (Dims ds -> Idxs ds -> Word) -> [Word]
check ds f = foldIdxs ds (\i xs -> f ds i : xs) []

pred_transpose_idxs' :: forall ds. Dims ds -> Bool
pred_transpose_idxs' ds = 
  check ds minorToMajor == check (T.unsafeReverse ds) (transposeIdxs minorToMajor)

pred_transpose_idxs :: forall ds. Dims ds -> Bool
pred_transpose_idxs ds = 
  check ds (withPerm (identity ds) minorToMajor) == check ds (withPerm (reversal ds) majorToMinor) &&
  check ds (withPerm (identity ds) majorToMinor) == check ds (withPerm (reversal ds) minorToMajor)


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

withPerm :: Perm (Rank ds) -> (Dims ds -> Idxs ds -> r) -> Dims ds -> Idxs ds -> r
withPerm p k d i = k (unsafePermute p d) (unsafePermute p i)

> withPerm (reversal d) minorToMajor d i
1
> withPerm (reversal d) majorToMinor d i
4
> withPerm mempty majorToMinor d i
1
> withPerm mempty minorToMajor d i
4

-}
