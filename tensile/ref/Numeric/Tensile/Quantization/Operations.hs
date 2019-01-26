module Numeric.Tensile.Quantization.Operations where

import Data.Vector (Vector)
import Data.Tensor.Types (Tensor)

round 
  :: forall d e i. RealFrac e 
  => Integral i 
  => Tensor d e
  -> Tensor d i
round t = undefined 
