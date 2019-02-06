module Data.Tensor 
  ( module Data.Tensor,
    module Data.Tensor.Internal
  )
where

-- TODO: reexport Types module
import Data.Tensor.Internal
import Data.Vector.Storable (Vector(..))
import Numeric.Backprop (BVar(..))
import Numeric.Tensile.Types


type T' s d = BVar s (T d)

-- | return the a runtime shape representing the dimensions of a tensor.
shape :: forall d t. KnownDims d => Tensor t d -> [Word]
shape _ = listDims (dims @_ @d)

-- | Product of all dimension sizes @O(Length xs)@.
size :: KnownDims d => Tensor t d -> Word
size = product . shape

fromVector' :: forall d t. (Elt t, KnownDims d) => Vector t -> Maybe (Tensor t d)
fromVector' = fromVector (dims @_ @d)

fromScalar :: Elt t => t -> Tensor t '[]
fromScalar = constant (dims @_ @'[])

constant :: Elt t => Dims d -> t -> Tensor t d
constant d t = fill d $ const t
