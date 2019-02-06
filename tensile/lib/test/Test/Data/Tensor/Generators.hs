module Test.Data.Tensor.Generators (
  module Test.Data.Tensor.Generators,
  module Test.Data.Tensor.Generators.Internal
) where

import Data.Tensor
import Data.Vector.Storable (Vector(..),Storable(..))
import Numeric.Tensile.Types (Dims(..), KnownDims(..), totalDim, dims)
import Test.Data.Tensor.Generators.Internal
import qualified Data.Vector.Storable as V

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

tensor' :: forall d e m. (MonadGen m, Elt e, KnownDims d) => (Range e -> m e) -> Range e -> m (Tensor d e)
tensor' = tensor $ dims @_ @d

