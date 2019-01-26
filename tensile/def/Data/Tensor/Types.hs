{-# LANGUAGE FlexibleInstances, KindSignatures, TypeOperators, UndecidableInstances #-}

module Data.Tensor.Types where

import Data.Bits
import Data.Singletons.Prelude.List (Product)
import Data.Vector.Storable (Vector)
import Eigen.Matrix
import GHC.TypeLits
import Numeric.Dimensions --(Dimensions(..), KnownDim(..), dimVal)

import qualified Data.Vector.Storable as V

-- TODO: move to application / test stanza
type TVal = Float
type IVal = Int


newtype Tensor (d :: [Nat]) e = Tensor { unTensor :: Vector e } deriving (Eq, Show)

-- | A real or complex-valued tensor of shape 'd'. 
type T d = Tensor d TVal

-- | An integer or non-negative integer-valued tensor of shape 'd'. 
type I d = Tensor d IVal

-- | A boolean-valued tensor of shape 'd'. 
type B d = Tensor d Bool



liftF f = Tensor . V.map f . unTensor

instance (KnownDim (Product d), V.Storable e, Eq e, Bits e, Num e) => Bits (Tensor d e) where
  Tensor a .&. Tensor b = Tensor $ V.zipWith (.&.) a b
  {-# INLINE (.&.) #-}
  Tensor a .|. Tensor b = Tensor $ V.zipWith (.|.) a b
  {-# INLINE (.|.) #-}
  Tensor a `xor` Tensor b = Tensor $ V.zipWith xor a b
  {-# INLINE xor #-}
  complement = liftF complement
  shift t i = liftF (flip shift i) t
  rotate t i = liftF (flip rotate i) t
  bit = Tensor . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Product d))) . bit
  testBit = testBitDefault
  bitSizeMaybe _ = bitSizeMaybe @e undefined
  bitSize _ = bitSize @e undefined
  isSigned _ = isSigned @e undefined
  popCount = popCountDefault

{-

instance (KnownDim (Product d), V.Storable e, Real e) => Real (Tensor d e) where
  toRational = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

instance (KnownDim (Product d), V.Storable e, Enum e) => Enum (Tensor d e) where
  toEnum = Tensor . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Product d))) . toEnum
  {-# INLINE toEnum #-}
  fromEnum = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

instance (KnownDim (Product d), V.Storable e, Integral e) => Integral (Tensor d e) where
  quot (Tensor a) (Tensor b) = Tensor $ V.zipWith quot a b
  rem (Tensor a) (Tensor b) = Tensor $ V.zipWith rem a b
  div (Tensor a) (Tensor b) = Tensor $ V.zipWith div a b
  mod (Tensor a) (Tensor b) = Tensor $ V.zipWith mod a b
  quotRem ta tb = (quot ta tb, rem ta tb)
  divMod ta tb = (div ta tb, mod ta tb)
  toInteger _ = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

-}

instance (KnownDim (Product d), V.Storable e, Num e) => Num (Tensor d e) where
  Tensor a + Tensor b = Tensor $ V.zipWith (+) a b
  {-# INLINE (+) #-}
  Tensor a - Tensor b = Tensor $ V.zipWith (-) a b
  {-# INLINE (-) #-}
  Tensor a * Tensor b = Tensor $ V.zipWith (*) a b
  {-# INLINE (*) #-}
  negate = liftF negate
  {-# INLINE negate #-}
  abs = liftF abs
  {-# INLINE abs #-}
  signum = liftF signum
  {-# INLINE signum #-}
  fromInteger = Tensor . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Product d))) . fromInteger
  {-# INLINE fromInteger #-}

instance (KnownDim (Product d), V.Storable e, Fractional e) => Fractional (Tensor d e) where
  recip = liftF recip
  {-# INLINE recip #-}
  Tensor a / Tensor b = Tensor $ V.zipWith (/) a b
  {-# INLINE (/) #-}
  fromRational = Tensor . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Product d))) . fromRational
  {-# INLINE fromRational #-}

instance (KnownDim (Product d), V.Storable e, Floating e) => Floating (Tensor d e) where
  pi = Tensor . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Product d))) $ pi
  {-# INLINE pi #-}
  exp = liftF exp
  {-# INLINE exp #-}
  sqrt = liftF sqrt
  {-# INLINE sqrt #-}
  log = liftF log
  {-# INLINE log #-}
  Tensor a ** Tensor b = Tensor $ V.zipWith (**) a b
  {-# INLINE (**) #-}
  logBase (Tensor a) (Tensor b) = Tensor $ V.zipWith logBase a b
  {-# INLINE logBase #-}
  sin = liftF sin
  {-# INLINE sin #-}
  tan = liftF tan
  {-# INLINE tan #-}
  cos = liftF cos
  {-# INLINE cos #-}
  asin = liftF asin
  {-# INLINE asin #-}
  atan = liftF atan
  {-# INLINE atan #-}
  acos = liftF acos
  {-# INLINE acos #-}
  sinh = liftF sinh
  {-# INLINE sinh #-}
  tanh = liftF tanh
  {-# INLINE tanh #-}
  cosh = liftF cosh
  {-# INLINE cosh #-}
  asinh = liftF asinh
  {-# INLINE asinh #-}
  atanh = liftF atanh
  {-# INLINE atanh #-}
  acosh = liftF acosh
  {-# INLINE acosh #-}

constant
  :: forall d e. V.Storable e
  => KnownDim (Product d)
  => Vector e
  -> Maybe (Tensor d e)
constant v
  | V.length v == fromIntegral (dimVal (dim :: Dim (Product d))) = Just $ Tensor v
  | otherwise = Nothing

equal
  :: V.Storable e
  => Eq e
  => Tensor d e
  -> Tensor d e
  -> Tensor d Bool
equal (Tensor a) (Tensor b) = Tensor $ V.zipWith (==) a b

notEqual
  :: V.Storable e
  => Eq e
  => Tensor d e
  -> Tensor d e
  -> Tensor d Bool
notEqual (Tensor a) (Tensor b) = Tensor $ V.zipWith (/=) a b

less
  :: V.Storable e
  => Ord e
  => Tensor d e
  -> Tensor d e
  -> Tensor d Bool
less (Tensor a) (Tensor b) = Tensor $ V.zipWith (<) a b

lessEqual
  :: V.Storable e
  => Ord e
  => Tensor d e
  -> Tensor d e
  -> Tensor d Bool
lessEqual (Tensor a) (Tensor b) = Tensor $ V.zipWith (<=) a b

greater
  :: V.Storable e
  => Ord e
  => Tensor d e
  -> Tensor d e
  -> Tensor d Bool
greater (Tensor a) (Tensor b) = Tensor $ V.zipWith (>) a b

greaterEqual
  :: V.Storable e
  => Ord e
  => Tensor d e
  -> Tensor d e
  -> Tensor d Bool
greaterEqual (Tensor a) (Tensor b) = Tensor $ V.zipWith (>=) a b

maximum
  :: V.Storable e
  => Ord e
  => Tensor d e
  -> Tensor d e
  -> Tensor d e
maximum (Tensor a) (Tensor b) = Tensor $ V.zipWith max a b

minimum
  :: V.Storable e
  => Ord e
  => Tensor d e
  -> Tensor d e
  -> Tensor d e
minimum (Tensor a) (Tensor b) = Tensor $ V.zipWith min a b
