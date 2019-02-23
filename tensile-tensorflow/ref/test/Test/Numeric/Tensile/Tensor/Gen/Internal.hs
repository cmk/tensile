module Test.Numeric.Tensile.Tensor.Gen.Internal (gen_tensor) where

import Data.Maybe (fromJust)
import Data.Vector.Storable (Vector(..),Storable(..))
import Numeric.Tensile.Dimensions (Dims(..), KnownDims(..), dims, fromDims)
import Numeric.Tensile.Tensor.Internal
import qualified Data.Vector.Storable as V

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

unsafeFromList :: forall d e. Elt e => Dims d -> [e] -> Tensor d e
unsafeFromList d = fromJust . fromList d

gen_tensor :: (Elt e, MonadGen m) => Dims d -> m e -> m (Tensor d e)
gen_tensor d g = unsafeFromList d <$> G.list r g
  where r = R.singleton $ fromIntegral $ product $ fromDims d
