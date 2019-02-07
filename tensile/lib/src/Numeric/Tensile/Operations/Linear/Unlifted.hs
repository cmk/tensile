module Numeric.Tensile.Operations.Linear.Unlifted (
  module Numeric.Tensile.Operations.Linear.Unlifted,
  module Numeric.Tensile.Operations.Linear.Internal
) where

import Numeric.Tensile.Permutation (Perm(..))
import Numeric.Tensile.Tensor
import Numeric.Tensile.Types
import Numeric.Tensile.Operations.Linear.Internal

transpose'
  :: forall d d' e. Elt e 
  => KnownDims d
  => Permutable d d'
  => Perm (Rank d) -> Tensor d e -> Tensor d' e
transpose' = transpose (dims @_ @d)

{-
infixl 7 <#>
(<#>)  
  :: forall a x y. KnownDim a
  => Dimensions x
  => Dimensions y
  => T (x +: a) -> T (a :+ y) -> T (x ++ y)
(<#>) = matmul
-}
