module Numeric.Tensile.Operations.Linear.Unlifted where

import Data.Tensor.Types
import Numeric.Dimensions
import Numeric.Tensile.Operations.Linear.Internal

{-
infixl 7 <#>
(<#>)  
  :: forall a x y. KnownDim a
  => Dimensions x
  => Dimensions y
  => T (x +: a) -> T (a :+ y) -> T (x ++ y)
(<#>) = matmul
-}
