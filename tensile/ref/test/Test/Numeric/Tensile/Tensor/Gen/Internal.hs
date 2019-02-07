module Test.Numeric.Tensile.Tensor.Gen.Internal where

import Numeric.Tensile.Tensor.Internal
import Data.Vector.Storable (Vector(..),Storable(..))
import Numeric.Tensile.Types (Dims(..), KnownDims(..), totalDim, dims)
import qualified Data.Vector.Storable as V

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

gen_vector_ranged :: (Storable e, MonadGen m) => Range Int -> m e -> m (Vector e)
gen_vector_ranged r g = V.fromList <$> G.list r g

gen_vector :: (Storable e, MonadGen m) => Range Int -> r -> (r -> m e) -> m (Vector e)
gen_vector ri r g = G.sized $ \n -> gen_vector_ranged ri (g r)

gen_tensor' :: (Elt e, MonadGen m) => Dims d -> Range e -> (Range e -> m e) -> m (Tensor d e)
gen_tensor' d r g = Tensor <$> gen_vector ri g r
  where ri = R.singleton $ fromIntegral (product $ listDims d)
