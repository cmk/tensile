module Numeric.Tensile.Tensor ( 
  module Numeric.Tensile.Tensor,
  module Numeric.Tensile.Tensor.Internal
) where

import Data.Vector.Sized (Vector)
import Numeric.Backprop (BVar(..))
import Numeric.Tensile.Dimensions
import Numeric.Tensile.Tensor.Internal

type T' s d = BVar s (T d)

-- | return the a runtime shape representing the dimensions of a tensor.
shape :: forall d e. KnownDims d => Tensor d e -> [Word]
shape _ = fromDims' (dims @d)

-- | Product of all dimension sizes @O(Length xs)@.
size :: KnownDims d => Tensor d e -> Word
size = product . shape

--fromVector' :: forall d e. (Elt e, KnownDims d) => Vector e -> Maybe (Tensor d e)
--fromVector' = fromVector (dims @d)

fromScalar :: Elt e => e -> Tensor '[] e
fromScalar = constant (dims @'[])

constant :: Elt e => Dims d -> e -> Tensor d e
constant d t = fill d $ const t
