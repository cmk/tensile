module Numeric.Tensile.Tensor 
  ( module Numeric.Tensile.Tensor,
    module Numeric.Tensile.Tensor.Internal
  )
where

-- TODO: reexport Types module
import Numeric.Tensile.Tensor.Internal
import Numeric.Backprop (BVar(..))
import Numeric.Tensile.Types

import Data.Vector.Sized (Vector)


type T' s d = BVar s (T d)

-- | return the a runtime shape representing the dimensions of a tensor.
shape :: forall d e. KnownDims d => Tensor d e -> [Word]
shape _ = listDims (dims @_ @d)

-- | Product of all dimension sizes @O(Length xs)@.
size :: KnownDims d => Tensor d e -> Word
size = product . shape

--fromVector' :: forall d e. (Elt e, KnownDims d) => Vector e -> Maybe (Tensor d e)
--fromVector' = fromVector (dims @_ @d)

fromScalar :: Elt e => e -> Tensor '[] e
fromScalar = constant (dims @_ @'[])

constant :: Elt e => Dims d -> e -> Tensor d e
constant d t = fill d $ const t
