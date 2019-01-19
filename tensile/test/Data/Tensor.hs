{-# LANGUAGE FlexibleInstances, KindSignatures, TypeFamilies, TypeOperators, UndecidableInstances #-}

--TODO rename to Data.Tensor.Sized
module Data.Tensor where

import Data.Singletons.Prelude.List (Product)
import Data.Vector (Vector)
import GHC.TypeLits
import Numeric.Dimensions --(Dimensions(..), KnownDim(..), dimVal)

import qualified Data.Vector as V

class Floating e => Elt e
instance Floating e => Elt e

data T (d :: [Nat]) e = T (Vector e)

constant
  :: forall d e. Elt e
  => Dimensions d
  => KnownDim (Product d)
  => Vector e
  -> Maybe (T d e)
constant v
  | V.length v == fromIntegral (dimVal (dim :: Dim (Product d))) = Just $ T v
  | otherwise = Nothing
