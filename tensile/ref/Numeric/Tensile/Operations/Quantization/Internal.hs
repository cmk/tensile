module Numeric.Tensile.Operations.Quantization.Internal where

import Data.Vector (Vector)
import Data.Tensor.Types (Tensor)

round 
  :: forall d e i. RealFrac e 
  => Integral i 
  => Tensor e d
  -> Tensor i d
round t = undefined 
