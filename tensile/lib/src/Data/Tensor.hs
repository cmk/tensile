module Data.Tensor 
  ( module Data.Tensor,
    module Data.Tensor.Internal
  )
where

-- TODO: reexport Types module
import Data.Tensor.Internal
import Numeric.Backprop (BVar(..))
import Numeric.Tensile.Types
import Unsafe.Coerce (unsafeCoerce)

type T' s d = BVar s (T d)

-- | return the a runtime shape representing the dimensions of a tensor.
shape :: forall d t. KnownDims d => Tensor t d -> [Word]
shape _ = listDims (dims @_ @d)

-- | Product of all dimension sizes @O(Length xs)@.
size :: KnownDims d => Tensor t d -> Word
size = product . shape

reshape 
  :: forall d d' t. Elt t 
  => Reshapable d d'
  => Tensor t d -> Tensor t d'
reshape = unsafeCoerce
