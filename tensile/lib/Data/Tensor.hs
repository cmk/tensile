module Data.Tensor 
  ( module Data.Tensor,
    module Data.Tensor.Types
  )
where

-- TODO: reexport Types module

import Data.Int (Int64)
import GHC.TypeLits (Nat)
import Numeric.Backprop (BVar(..))
import Numeric.Dimensions (Dimensions(..))
import qualified Numeric.Dimensions as D

import Data.Tensor.Types



type T' s d = BVar s (T d)

-- | return the a runtime shape representing the dimensions of a tensor.
shape :: forall d e. Dimensions d => Tensor e d -> [Int64]
shape _ = fromIntegral <$> D.listDims (D.dims @Nat @d)

-- | Product of all dimension sizes @O(Length xs)@.
size :: Dimensions d => Tensor e d -> Int64
size = product . shape

{-
vector :: forall n . KnownDim n => KnownNat n => [HsReal] -> ExceptT String IO (Tensor '[n])
vector rs
  | genericLength rs == dimVal (dim :: Dim n) = asStatic <$> Dynamic.vectorEIO rs
  | otherwise = ExceptT . pure $ Left "Vector dimension does not match length of list"

unsafeVector :: (KnownDim n, KnownNat n) => [HsReal] -> IO (Tensor '[n])
unsafeVector = fmap (either error id) . runExceptT . vector
-}
