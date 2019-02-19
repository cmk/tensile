module Test.Numeric.Tensile.Dimensions.Dims.Predicate where

import Data.Functor.Identity
import Numeric.Tensile.Dimensions.Dims
import Numeric.Tensile.Dimensions.Types

reifyReflect :: Dims ds -> Dims ds
reifyReflect ds = reifyDims ds (reflectDims id)

pred_reify_reflect :: Dims ds -> Bool
pred_reify_reflect ds = reifyReflect ds == ds

evidenceReflect :: Dims ds -> Dims ds
evidenceReflect ds = withEvidence (withDims ds) (reflectDims id)

pred_evidence_reflect :: Dims ds -> Bool
pred_evidence_reflect ds = evidenceReflect ds == ds 

traverseSomeDims :: SomeDims -> [Word]
traverseSomeDims = runIdentity . traverse (\s -> Identity $ withSomeDim s fromDim) 

pred_traverse_somedims :: SomeDims -> Bool
pred_traverse_somedims ds = withSomeDims ds fromDims == traverseSomeDims ds

{-

 
pred_traverse_dims :: Dims ds -> Bool
pred_traverse_dims ds = evidenceReflect ds == ds 

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


-}
