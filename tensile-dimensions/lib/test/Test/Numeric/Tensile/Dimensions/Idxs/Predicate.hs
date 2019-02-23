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
> check d (majorToMinor)
[7,5,3,1,6,4,2,0]
> check d (transposeIdxs majorToMinor)
[7,3,6,2,5,1,4,0]
> check d (minorToMajor)
[7,6,5,4,3,2,1,0]
> check d (transposeIdxs minorToMajor)
[7,6,5,4,3,2,1,0]

> check ds (withPerm (identity ds) majorToMinor)
[7,5,3,1,6,4,2,0]
> check ds (withPerm (reversal ds) majorToMinor)
[7,6,5,4,3,2,1,0]
> check ds (withPerm (identity ds) minorToMajor)
[7,6,5,4,3,2,1,0]
> check ds (withPerm (reversal ds) minorToMajor)
[7,5,3,1,6,4,2,0]


-}
