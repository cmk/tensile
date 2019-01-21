module Numeric.Tensile.Quantization.Operations where

import Data.Vector (Vector)
import Data.Tensor.Types (T(..), Elt(..))

round 
  :: forall d e i. Elt e
  => Elt i
  => RealFrac e 
  => Integral i 
  => T d e
  -> T d i
round t = undefined 
