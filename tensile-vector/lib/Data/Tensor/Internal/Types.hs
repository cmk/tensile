{-# LANGUAGE FlexibleInstances, KindSignatures, TypeOperators, UndecidableInstances #-}

module Data.Tensor.Internal.Types where

import Data.Bits
import Data.Singletons.Prelude.List (Product)
import Data.Vector (Vector)
import GHC.TypeLits
import Numeric.Dimensions --(Dimensions(..), KnownDim(..), dimVal)

import qualified Data.Vector as V

class Num e => Elt e
instance Num e => Elt e

data T (d :: [Nat]) e = T (Vector e) deriving (Eq, Show)

instance Functor (T d) where
  fmap f (T a) = T (fmap f a)
  {-# INLINE fmap #-}

instance (KnownDim (Product d), Elt e, Eq e, Bits e, Num e) => Bits (T d e) where
  T a .&. T b = T $ V.zipWith (.&.) a b
  {-# INLINE (.&.) #-}
  T a .|. T b = T $ V.zipWith (.|.) a b
  {-# INLINE (.|.) #-}
  T a `xor` T b = T $ V.zipWith xor a b
  {-# INLINE xor #-}
  complement = fmap complement
  shift t i = fmap (flip shift i) t
  rotate t i = fmap (flip rotate i) t
  bit = T . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Product d))) . bit
  testBit = testBitDefault
  bitSizeMaybe _ = bitSizeMaybe @e undefined
  bitSize _ = bitSize @e undefined
  isSigned _ = isSigned @e undefined
  popCount = popCountDefault

{-

instance (KnownDim (Product d), Elt e, Real e) => Real (T d e) where
  toRational = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

instance (KnownDim (Product d), Elt e, Enum e) => Enum (T d e) where
  toEnum = T . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Product d))) . toEnum
  {-# INLINE toEnum #-}
  fromEnum = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

instance (KnownDim (Product d), Elt e, Integral e) => Integral (T d e) where
  quot (T a) (T b) = T $ V.zipWith quot a b
  rem (T a) (T b) = T $ V.zipWith rem a b
  div (T a) (T b) = T $ V.zipWith div a b
  mod (T a) (T b) = T $ V.zipWith mod a b
  quotRem ta tb = (quot ta tb, rem ta tb)
  divMod ta tb = (div ta tb, mod ta tb)
  toInteger _ = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

-}

instance (KnownDim (Product d), Elt e, Num e) => Num (T d e) where
  T a + T b = T $ V.zipWith (+) a b
  {-# INLINE (+) #-}
  T a - T b = T $ V.zipWith (-) a b
  {-# INLINE (-) #-}
  T a * T b = T $ V.zipWith (*) a b
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
  T a / T b = T $ V.zipWith (/) a b
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
  T a ** T b = T $ V.zipWith (**) a b
  {-# INLINE (**) #-}
  logBase (T a) (T b) = T $ V.zipWith logBase a b
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

equal
  :: forall d e. Elt e
  => Eq e
  => T d e
  -> T d e
  -> T d Bool
equal (T a) (T b) = T $ V.zipWith (==) a b

notEqual
  :: forall d e. Elt e
  => Eq e
  => T d e
  -> T d e
  -> T d Bool
notEqual (T a) (T b) = T $ V.zipWith (/=) a b

less
  :: forall d e. Elt e
  => Ord e
  => T d e
  -> T d e
  -> T d Bool
less (T a) (T b) = T $ V.zipWith (<) a b

lessEqual
  :: forall d e. Elt e
  => Ord e
  => T d e
  -> T d e
  -> T d Bool
lessEqual (T a) (T b) = T $ V.zipWith (<=) a b

greater
  :: forall d e. Elt e
  => Ord e
  => T d e
  -> T d e
  -> T d Bool
greater (T a) (T b) = T $ V.zipWith (>) a b

greaterEqual
  :: forall d e. Elt e
  => Ord e
  => T d e
  -> T d e
  -> T d Bool
greaterEqual (T a) (T b) = T $ V.zipWith (>=) a b

maximum
  :: forall d e. Elt e
  => Ord e
  => T d e
  -> T d e
  -> T d e
maximum (T a) (T b) = T $ V.zipWith max a b

minimum
  :: forall d e. Elt e
  => Ord e
  => T d e
  -> T d e
  -> T d e
minimum (T a) (T b) = T $ V.zipWith min a b
