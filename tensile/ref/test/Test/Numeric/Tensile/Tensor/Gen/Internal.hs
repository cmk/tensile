module Test.Numeric.Tensile.Tensor.Gen.Internal where

import Numeric.Tensile.Tensor.Internal
import Data.Vector.Storable (Vector(..),Storable(..))
import Numeric.Tensile.Types (Dims(..), KnownDims(..), totalDim, dims)
import qualified Data.Vector.Storable as V

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

_vector :: (MonadGen m, Storable e) => Range Int -> m e -> m (Vector e)
_vector r g = V.fromList <$> G.list r g

gen_vector :: (MonadGen m, Storable e) => Range Int -> (r -> m e) -> r -> m (Vector e)
gen_vector ran gen size = G.sized $ \n -> _vector ran (gen size)

gen_tensor' :: (MonadGen m, Elt e) => Dims d -> (Range e -> m e) -> Range e -> m (Tensor d e)
gen_tensor' d g r = Tensor <$> gen_vector ran g r
  where ran = R.singleton $ fromIntegral (totalDim d)
