module Test.Numeric.Tensile.Dimensions.Dims.Predicate where

import Numeric.Tensile.Dimensions
import Test.Numeric.Tensile.Dimensions.Gen

pred_max_diffIdx :: forall (d :: [Nat]). Dims d -> Bool
pred_max_diffIdx d = (fromIntegral $ 1 + diffIdxs d (maxBound' d) (minBound' d)) == (product . fromDims $ d)


{-
 -
-- test -

reifyReflect :: Dim d -> Dim d
reifyReflect d = reifyDim d (reflectDim id)

-- todo : prove 
reifyDims d == withEvidence (withDims d) 


-}
