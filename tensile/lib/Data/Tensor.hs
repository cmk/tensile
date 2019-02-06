module Data.Tensor 
  ( module Data.Tensor,
    module Data.Tensor.Types
  )
where

-- TODO: reexport Types module

import Data.Int (Int64)
import Numeric.Backprop (BVar(..))
import Numeric.Dimensions (Nat, Reverse, Dimensions)
import qualified Numeric.Dimensions as D

import Data.Tensor.Types

type T' s d = BVar s (T d)

-- | return the a runtime shape representing the dimensions of a tensor.
shape :: forall t ds. Dimensions ds => Tensor t ds -> [Int64]
shape _ = fromIntegral <$> D.listDims (D.dims @_ @ds)

-- | Product of all dimension sizes @O(Length xs)@.
size :: Dimensions ds => Tensor t ds -> Int64
size = product . shape


