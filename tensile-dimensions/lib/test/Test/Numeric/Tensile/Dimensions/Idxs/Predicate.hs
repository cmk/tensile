module Test.Numeric.Tensile.Dimensions.Idxs.Predicate where

import Numeric.Tensile.Dimensions
import qualified Numeric.Tensile.Dimensions.Types as T

import Debug.Trace

-- | Property that the number of elements in the tensor is equal to the size
--   of the tensor.
pred_size_idxs :: forall d. Dims d -> Bool
pred_size_idxs d = size d == foldIdxs d (\_ a -> a + 1) 0

--  | Property that the number of elements in the tensor is one more than the
--    difference between the largest index and the smallest index.
pred_max_diff_idxs :: forall d. Dims d -> Bool
pred_max_diff_idxs d = size d == s
  where s = 1 + diffIdxs d (maxBound' d) (minBound' d)

--  | Property that the number of elements in the tensor is one more than the
--    largest index.
pred_max_diff_idxs2 :: forall d. Dims d -> Bool
pred_max_diff_idxs2 d = size d == 1 + s
  where s = fromIdxs d $ idxs d (-1)

-- | Property that rotating an index by the size of the space leaves it unchanged.
pred_modulus_idxs :: Dims d -> Bool
pred_modulus_idxs d = foldIdxs d k True
  where k i b = b && liftIdxs d (+ size d) i == i

check :: Dims d -> (Dims d -> Idxs d -> Word) -> [Word]
check d f = foldIdxs d (\i xs -> f d i : xs) []

-- | Property that minor-to-major (e.g. row-major) indexing on 'Dims d' is identical
--   to major-to-minor (e.g. column-major) indexing on 'Dims (Reverse d)'.
pred_transpose_idxs :: forall d. Dims d -> Bool
pred_transpose_idxs d = 
  check d (withPerm (identity d) minorToMajor) == check d (withPerm (reversal d) majorToMinor) &&
  check d (withPerm (identity d) majorToMinor) == check d (withPerm (reversal d) minorToMajor)

pred_transpose_idxs' :: forall d. Dims d -> Bool
pred_transpose_idxs' d = 
  check d minorToMajor == check (T.unsafeReverse d) (transposeIdxs minorToMajor)


--TODO add arbitrary permutations
--pred_permute_idxs :: forall d. Dims d -> Bool

{- TODO is this sketchy behavior?
> check d (majorToMinor)
[7,5,3,1,6,4,2,0]
> check d (transposeIdxs majorToMinor)
[7,3,6,2,5,1,4,0]
> check d (minorToMajor)
[7,6,5,4,3,2,1,0]
> check d (transposeIdxs minorToMajor)
[7,6,5,4,3,2,1,0]

> check d (withPerm (identity d) majorToMinor)
[7,5,3,1,6,4,2,0]
> check d (withPerm (reversal d) majorToMinor)
[7,6,5,4,3,2,1,0]
> check d (withPerm (identity d) minorToMajor)
[7,6,5,4,3,2,1,0]
> check d (withPerm (reversal d) minorToMajor)
[7,5,3,1,6,4,2,0]


-}
