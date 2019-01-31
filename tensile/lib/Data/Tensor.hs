{-# LANGUAGE MagicHash              #-}
module Data.Tensor 
  ( module Data.Tensor,
    module Data.Tensor.Types
  )
where

-- TODO: reexport Types module

import Data.Int (Int64)
import GHC.Base (unsafeCoerce#)
import GHC.TypeLits (Nat)
import Numeric.Backprop (BVar(..))
import Numeric.Dimensions (Reverse,Dimensions)
import qualified Numeric.Dimensions as D

import Data.Tensor.Types

type Idxs ds = D.Idxs (Reverse ds)
type Dims ds = D.Dims (Reverse ds)

--TODO: don't specialize to [Nat]
--      figure out how to reverse these lists!
--
reverseIdxs :: Idxs ds -> D.Idxs (Reverse ds)
reverseIdxs dims = unsafeCoerce# (reverse (unsafeCoerce# dims))
{-# INLINE reverseIdxs #-}

reverseDims :: Dims ds -> D.Dims (Reverse ds)
reverseDims dims = unsafeCoerce# (reverse (unsafeCoerce# dims))
{-# INLINE reverseDims #-}


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
