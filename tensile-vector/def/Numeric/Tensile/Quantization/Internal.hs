module Numeric.Tensile.Quantization.Internal (round) where

import Data.Vector (Vector)
import Data.Tensor.Internal (T(..), Elt(..))
import qualified Prelude as P

round
  :: forall d e i. Elt e
  => Elt i
  => P.RealFrac e
  => P.Integral i 
  => T d e
  -> T d i
round t = P.undefined
