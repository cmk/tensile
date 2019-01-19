{-# LANGUAGE FlexibleInstances, KindSignatures, TypeFamilies, TypeOperators, UndecidableInstances #-}

--TODO rename to Data.Tensor.Sized
module Data.Tensor where

import Data.Singletons.Prelude.List (Product)
import Data.Vector (Vector)
import GHC.TypeLits
import Numeric.Dimensions --(Dimensions(..), KnownDim(..), dimVal)

import qualified Data.Vector as V

class Num e => Elt e
instance Num e => Elt e

data T (d :: [Nat]) e = T (Vector e) deriving (Eq, Show)

instance Functor (T d) where
  fmap f (T as) = T (fmap f as)
  {-# INLINE fmap #-}


instance (Elt e, Num e, KnownDim (Product d)) => Num (T d e) where
  T as + T bs = T $ V.zipWith (+) as bs
  {-# INLINE (+) #-}
  T as - T bs = T $ V.zipWith (-) as bs
  {-# INLINE (-) #-}
  T as * T bs = T $ V.zipWith (*) as bs
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = T . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Product d))) . fromInteger
  {-# INLINE fromInteger #-}

constant
  :: forall d e. Elt e
  => KnownDim (Product d)
  => Vector e
  -> Maybe (T d e)
constant v
  | V.length v == fromIntegral (dimVal (dim :: Dim (Product d))) = Just $ T v
  | otherwise = Nothing
