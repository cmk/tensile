{-# LANGUAGE FlexibleInstances, KindSignatures, TypeOperators, UndecidableInstances #-}

module Data.Tensor.Types where

import Data.Bits
import Data.Singletons.Prelude.List (Product)
import Data.Vector (Vector)
import GHC.TypeLits
import Numeric.Dimensions --(Dimensions(..), KnownDim(..), dimVal)

import qualified Data.Vector as V

type TVal = Float
type IVal = Int

newtype Tensor (d :: [Nat]) e = Tensor (Vector e) deriving (Eq, Show)

instance Functor (Tensor d) where
  fmap f (Tensor a) = Tensor (fmap f a)
  {-# INLINE fmap #-}

instance (KnownDim (Product d), Eq e, Bits e, Num e) => Bits (Tensor d e) where
  Tensor a .&. Tensor b = Tensor $ V.zipWith (.&.) a b
  {-# INLINE (.&.) #-}
  Tensor a .|. Tensor b = Tensor $ V.zipWith (.|.) a b
  {-# INLINE (.|.) #-}
  Tensor a `xor` Tensor b = Tensor $ V.zipWith xor a b
  {-# INLINE xor #-}
  complement = fmap complement
  shift t i = fmap (flip shift i) t
  rotate t i = fmap (flip rotate i) t
  bit = Tensor . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Product d))) . bit
  testBit = testBitDefault
  bitSizeMaybe _ = bitSizeMaybe @e undefined
  bitSize _ = bitSize @e undefined
  isSigned _ = isSigned @e undefined
  popCount = popCountDefault

{-

instance (KnownDim (Product d), Real e) => Real (Tensor d e) where
  toRational = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

instance (KnownDim (Product d), Enum e) => Enum (Tensor d e) where
  toEnum = Tensor . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Product d))) . toEnum
  {-# INLINE toEnum #-}
  fromEnum = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

instance (KnownDim (Product d), Integral e) => Integral (Tensor d e) where
  quot (Tensor a) (Tensor b) = Tensor $ V.zipWith quot a b
  rem (Tensor a) (Tensor b) = Tensor $ V.zipWith rem a b
  div (Tensor a) (Tensor b) = Tensor $ V.zipWith div a b
  mod (Tensor a) (Tensor b) = Tensor $ V.zipWith mod a b
  quotRem ta tb = (quot ta tb, rem ta tb)
  divMod ta tb = (div ta tb, mod ta tb)
  toInteger _ = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

-}

instance (KnownDim (Product d), Num e) => Num (Tensor d e) where
  Tensor a + Tensor b = Tensor $ V.zipWith (+) a b
  {-# INLINE (+) #-}
  Tensor a - Tensor b = Tensor $ V.zipWith (-) a b
  {-# INLINE (-) #-}
  Tensor a * Tensor b = Tensor $ V.zipWith (*) a b
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = Tensor . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Product d))) . fromInteger
  {-# INLINE fromInteger #-}

instance (KnownDim (Product d), Fractional e) => Fractional (Tensor d e) where
  recip = fmap recip
  {-# INLINE recip #-}
  Tensor a / Tensor b = Tensor $ V.zipWith (/) a b
  {-# INLINE (/) #-}
  fromRational = Tensor . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Product d))) . fromRational
  {-# INLINE fromRational #-}

instance (KnownDim (Product d), Floating e) => Floating (Tensor d e) where
  pi = Tensor . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Product d))) $ pi
  {-# INLINE pi #-}
  exp = fmap exp
  {-# INLINE exp #-}
  sqrt = fmap sqrt
  {-# INLINE sqrt #-}
  log = fmap log
  {-# INLINE log #-}
  Tensor a ** Tensor b = Tensor $ V.zipWith (**) a b
  {-# INLINE (**) #-}
  logBase (Tensor a) (Tensor b) = Tensor $ V.zipWith logBase a b
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
  :: forall d e. KnownDim (Product d)
  => Vector e
  -> Maybe (Tensor d e)
constant v
  | V.length v == fromIntegral (dimVal (dim :: Dim (Product d))) = Just $ Tensor v
  | otherwise = Nothing

equal
  :: Eq e
  => Tensor d e
  -> Tensor d e
  -> Tensor d Bool
equal (Tensor a) (Tensor b) = Tensor $ V.zipWith (==) a b

notEqual
  :: Eq e
  => Tensor d e
  -> Tensor d e
  -> Tensor d Bool
notEqual (Tensor a) (Tensor b) = Tensor $ V.zipWith (/=) a b

less
  :: Ord e
  => Tensor d e
  -> Tensor d e
  -> Tensor d Bool
less (Tensor a) (Tensor b) = Tensor $ V.zipWith (<) a b

lessEqual
  :: Ord e
  => Tensor d e
  -> Tensor d e
  -> Tensor d Bool
lessEqual (Tensor a) (Tensor b) = Tensor $ V.zipWith (<=) a b

greater
  :: Ord e
  => Tensor d e
  -> Tensor d e
  -> Tensor d Bool
greater (Tensor a) (Tensor b) = Tensor $ V.zipWith (>) a b

greaterEqual
  :: Ord e
  => Tensor d e
  -> Tensor d e
  -> Tensor d Bool
greaterEqual (Tensor a) (Tensor b) = Tensor $ V.zipWith (>=) a b

maximum
  :: Ord e
  => Tensor d e
  -> Tensor d e
  -> Tensor d e
maximum (Tensor a) (Tensor b) = Tensor $ V.zipWith max a b

minimum
  :: Ord e
  => Tensor d e
  -> Tensor d e
  -> Tensor d e
minimum (Tensor a) (Tensor b) = Tensor $ V.zipWith min a b
