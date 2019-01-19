module Numeric.Tensile.Tensor 
  ( module Numeric.Tensile.Tensor,
    module Numeric.Tensile.Tensor.Types
  )
where

-- TODO: reexport Types module

import Data.Int (Int64)
import GHC.TypeLits (Nat)
import Numeric.Dimensions (Dimensions(..))
import qualified Numeric.Dimensions as D

import Numeric.Tensile.Tensor.Types

-- | return the a runtime shape representing the dimensions of a tensor.
shape :: forall s e. Dimensions s => T s e -> [Int64]
shape _ = fromIntegral <$> D.listDims (D.dims @Nat @s)

-- | Product of all dimension sizes @O(Length xs)@.
size :: forall s e. Dimensions s => T s e -> Int64
size = product . shape

{-
vector :: forall n . KnownDim n => KnownNat n => [HsReal] -> ExceptT String IO (Tensor '[n])
vector rs
  | genericLength rs == dimVal (dim :: Dim n) = asStatic <$> Dynamic.vectorEIO rs
  | otherwise = ExceptT . pure $ Left "Vector dimension does not match length of list"

unsafeVector :: (KnownDim n, KnownNat n) => [HsReal] -> IO (Tensor '[n])
unsafeVector = fmap (either error id) . runExceptT . vector
-}
