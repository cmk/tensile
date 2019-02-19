module Test.Numeric.Tensile.Dimensions.Dims.Predicate where

import Numeric.Tensile.Dimensions.Dims
import Numeric.Tensile.Dimensions.Types

reifyReflect :: Dims d -> Dims d
reifyReflect d = reifyDims d (reflectDims id)

pred_reify_reflect :: forall (d :: [Nat]). Dims d -> Bool
pred_reify_reflect d = reifyReflect d == d

{-

pred_reify_evidence :: forall (d :: [Nat]). Dims d -> Bool
pred_reify_evidence d = reifyDims d == withEvidence (withDims d)


TODO: Add prop tests
> ds = dims @'[1,2,3]
> traverseDims (pure . SomeDim) ds
[SomeDim 1,SomeDim 2,SomeDim 3]

> sd = fromJust $ someDims [1,2,3]
> traverseDims' pure sd
[SomeDim 1,SomeDim 2,SomeDim 3]

> ds = dims @'[1,2,3]
> mapDims SomeDim ds
[SomeDim 1,SomeDim 2,SomeDim 3]

> sd = fromJust $ someDims [1,2,3]
> traverseDims' pure sd
[SomeDim 1,SomeDim 2,SomeDim 3]

traverse (\s -> pure $ withSomeDim s fromDim) sd == withSomeDims sd fromDims

-}
