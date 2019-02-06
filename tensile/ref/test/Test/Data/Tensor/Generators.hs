module Test.Data.Tensor.Generators where

import Data.Tensor.Internal
import Data.Vector.Storable (Vector(..),Storable(..))
import Numeric.Tensile.Types (Dims(..), KnownDims(..), totalDim, dims)
import qualified Data.Vector.Storable as V

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

rf :: Range TVal
rf = R.linearFracFrom 0 (-100) 100

_vector :: (MonadGen m, Storable t) => Range Int -> m t -> m (Vector t)
_vector r g = V.fromList <$> G.list r g

genVectorOf :: Storable t => Range Int -> (e -> Gen t) -> e -> Gen (Vector t)
genVectorOf ran gen size = G.sized $ \n -> _vector ran (gen size)

tensor' :: forall d. KnownDims d => Range TVal -> Gen (T d)
tensor' = tensor $ dims @_ @d

tensor :: Dims d -> Range TVal -> Gen (T d)
tensor d r = Tensor <$> genVectorOf ran G.float r
  where ran = R.singleton $ fromIntegral (totalDim d)

--r = (R.linear 1 (fromIntegral n)) 
