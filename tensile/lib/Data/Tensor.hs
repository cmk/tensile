module Data.Tensor 
  ( module Data.Tensor,
    module Data.Tensor.Types
  )
where

-- TODO: reexport Types module

import Numeric.Backprop (BVar(..))
import Numeric.Tensile.Types
import Data.Tensor.Types

type T' s d = BVar s (T d)

-- | return the a runtime shape representing the dimensions of a tensor.
shape :: forall d t. KnownDims d => Tensor t d -> [Word]
shape _ = listDims (dims @_ @d)

-- | Product of all dimension sizes @O(Length xs)@.
size :: KnownDims d => Tensor t d -> Word
size = product . shape


