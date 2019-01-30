module Numeric.Tensile.Operations.Linear.Unlifted where

import Data.Tensor.Types
import Numeric.Dimensions
import Numeric.Tensile.Operations.Linear.Internal

infixl 7 <#>
(<#>)  
  :: forall a b ab x. KnownDim x
  => ConcatList a b ab -- witness 'a ++ b ~ ab'
  => Dimensions a
  => Dimensions b
  => T (a +: x) -> T (x :+ b) -> T ab
(<#>) = matmul
