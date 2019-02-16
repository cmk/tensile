module Test.Numeric.Tensile.Tensor.Gen.Internal (gen_tensor') where

import Numeric.Tensile.Tensor.Internal
import Data.Vector.Storable (Vector(..),Storable(..))
import Numeric.Tensile.Types (Dims(..), KnownDims(..), listDims, dims)
import qualified Data.Vector.Storable as V

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

gen_vector :: (Storable e, MonadGen m) => Range Int -> m e -> m (Vector e)
gen_vector r g = V.fromList <$> G.list r g

gen_tensor' :: (Elt e, MonadGen m) => Dims d -> m e -> m (Tensor d e)
gen_tensor' d g = Tensor <$> gen_vector r g
  where r = R.singleton $ fromIntegral (product $ listDims d)
