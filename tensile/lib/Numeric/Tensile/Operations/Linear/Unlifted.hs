module Numeric.Tensile.Operations.Linear.Unlifted where

import Data.Tensor.Types
import Numeric.Dimensions
import Numeric.Tensile.Operations.Linear.Internal

infixl 7 <#>
(<#>)  
  :: forall a b x. KnownDim x
  => Dimensions a
  => Dimensions b
  => T (a +: x) -> T (x :+ b) -> T (a ++ b)
(<#>) = matmul
