module Test.Numeric.Tensile.Tensor.Gen.Internal (gen_tensor') where

import Numeric.Tensile.Tensor.Internal
import Data.Vector.Storable (Vector(..),Storable(..))
import Numeric.Tensile.Dimensions.Types (Dims(..), KnownDims(..), listDims, dims)

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R


gen_tensor' :: (Elt e, MonadGen m) => Dims d -> m e -> m (Tensor d e)
gen_tensor' d g = undefined
