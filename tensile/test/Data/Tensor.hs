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


instance (KnownDim (Product d), Elt e, Num e) => Num (T d e) where
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

instance (KnownDim (Product d), Elt e, Fractional e) => Fractional (T d e) where
  recip = fmap recip
  {-# INLINE recip #-}
  T as / T bs = T $ V.zipWith (/) as bs
  {-# INLINE (/) #-}
  fromRational = T . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Product d))) . fromRational
  {-# INLINE fromRational #-}

instance (KnownDim (Product d), Elt e, Floating e) => Floating (T d e) where
  pi =  T . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Product d))) $ pi
  {-# INLINE pi #-}
  exp = fmap exp
  {-# INLINE exp #-}
  sqrt = fmap sqrt
  {-# INLINE sqrt #-}
  log = fmap log
  {-# INLINE log #-}
  T as ** T bs = T $ V.zipWith (**) as bs
  {-# INLINE (**) #-}
  logBase (T as) (T bs) = T $ V.zipWith logBase as bs
  {-# INLINE logBase #-}
  sin = fmap sin
  {-# INLINE sin #-}
  tan = fmap tan
  {-# INLINE tan #-}
  cos = fmap cos
  {-# INLINE cos #-}
  asin = fmap asin
  {-# INLINE asin #-}
  atan = fmap atan
  {-# INLINE atan #-}
  acos = fmap acos
  {-# INLINE acos #-}
  sinh = fmap sinh
  {-# INLINE sinh #-}
  tanh = fmap tanh
  {-# INLINE tanh #-}
  cosh = fmap cosh
  {-# INLINE cosh #-}
  asinh = fmap asinh
  {-# INLINE asinh #-}
  atanh = fmap atanh
  {-# INLINE atanh #-}
  acosh = fmap acosh
  {-# INLINE acosh #-}

constant
  :: forall d e. Elt e
  => KnownDim (Product d)
  => Vector e
  -> Maybe (T d e)
constant v
  | V.length v == fromIntegral (dimVal (dim :: Dim (Product d))) = Just $ T v
  | otherwise = Nothing
