module Numeric.Tensile.Quantization.Internal.Types where

import Data.Vector (Vector)
import Data.Tensor.Internal.Types (T(..), Elt(..))

round 
  :: forall d e i. Elt e
  => Elt i
  => RealFrac e 
  => Integral i 
  => T d e
  -> T d i
round t = undefined 

foo :: Int
foo = 2 
