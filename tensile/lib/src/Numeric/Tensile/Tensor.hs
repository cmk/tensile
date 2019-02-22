module Numeric.Tensile.Tensor ( 
  module Numeric.Tensile.Tensor,
  module Numeric.Tensile.Tensor.Internal
) where

import Data.Vector.Sized (Vector)
import Numeric.Backprop (BVar(..))
import Numeric.Tensile.Dimensions
import Numeric.Tensile.Tensor.Internal
import Unsafe.Coerce (unsafeCoerce)

type T' s d = BVar s (T d)

reshape :: forall d d' e. Elt e => Reshapable d d' => Tensor d e -> Tensor d' e
reshape = unsafeCoerce

-- | return the a runtime shape representing the dimensions of a tensor.
shape :: forall d e. KnownDims d => Tensor d e -> [Word]
shape _ = fromDims (dims @d)

-- | Product of all dimension sizes @O(Length xs)@.
size :: KnownDims d => Tensor d e -> Word
size = product . shape

--fromVector' :: forall d e. (Elt e, KnownDims d) => Vector e -> Maybe (Tensor d e)
--fromVector' = fromVector (dims @d)

fromScalar :: Elt e => e -> Tensor '[] e
fromScalar = constant (dims @'[])

