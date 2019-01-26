{-# LANGUAGE FlexibleInstances, KindSignatures, TypeOperators, UndecidableInstances #-}

module Data.Tensor.Types where

import Data.Bits
import Data.Singletons.Prelude.List (Product)
import Data.Vector.Storable (Vector)
import GHC.TypeLits
import Numeric.Dimensions --(Dimensions(..), KnownDim(..), dimVal)

import qualified Data.Vector.Storable as V
import qualified Eigen.Matrix as E

-- TODO: move to application / test stanza
type TVal = Float
type IVal = Int

type Elt = V.Storable

newtype Tensor (d :: [Nat]) e = Tensor { unTensor :: Vector e } deriving (Eq, Show)

toVec 
  :: forall d e n. E.Elem e 
  => E.C e ~ e
  => Product d ~ n
  => Tensor d e 
  -> E.Vec n e
toVec = E.Vec . unTensor

toMatrix
  :: forall d e m n. E.Elem e 
  => E.C e ~ e
  => Product d ~ (n * m)
  => Tensor d e
  -> E.Matrix n m e
toMatrix = E.Matrix . toVec

-- | A real or complex-valued tensor of shape 'd'. 
type T d = Tensor d TVal

-- | An integer or non-negative integer-valued tensor of shape 'd'. 
type I d = Tensor d IVal

-- | A boolean-valued tensor of shape 'd'. 
type B d = Tensor d Bool


liftF
  :: (V.Storable a, V.Storable e) =>
     (a -> e) -> Tensor d a -> Tensor d e
liftF f = Tensor . V.map f . unTensor

liftF2
  :: (V.Storable a, V.Storable b, V.Storable e) =>
     (a -> b -> e) -> Tensor d a -> Tensor d b -> Tensor d e
liftF2 f (Tensor a) (Tensor b) = Tensor $ V.zipWith f a b


instance (KnownDim (Product d), Elt e, Eq e, Bits e, Num e) => Bits (Tensor d e) where
  (.&.) = liftF2 (.&.)
  {-# INLINE (.&.) #-}
  (.|.) = liftF2 (.|.)
  {-# INLINE (.|.) #-}
  xor = liftF2 xor
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

instance (KnownDim (Product d), Elt e, Real e) => Real (Tensor d e) where
  toRational = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

instance (KnownDim (Product d), Elt e, Enum e) => Enum (Tensor d e) where
  toEnum = Tensor . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Product d))) . toEnum
  {-# INLINE toEnum #-}
  fromEnum = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

instance (KnownDim (Product d), Elt e, Integral e) => Integral (Tensor d e) where
  quot (Tensor a) (Tensor b) = liftF2 quot a b
  rem (Tensor a) (Tensor b) = liftF2 rem a b
  div (Tensor a) (Tensor b) = liftF2 div a b
  mod (Tensor a) (Tensor b) = liftF2 mod a b
  quotRem ta tb = (quot ta tb, rem ta tb)
  divMod ta tb = (div ta tb, mod ta tb)
  toInteger _ = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

-}

instance (KnownDim (Product d), Elt e, Num e) => Num (Tensor d e) where
  (+) = liftF2 (+)
  {-# INLINE (+) #-}
  (-) = liftF2 (-)
  {-# INLINE (-) #-}
  (*) = liftF2 (*)
  {-# INLINE (*) #-}
  negate = liftF negate
  {-# INLINE negate #-}
  abs = liftF abs
  {-# INLINE abs #-}
  signum = liftF signum
  {-# INLINE signum #-}
  fromInteger = Tensor . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Product d))) . fromInteger
  {-# INLINE fromInteger #-}

instance (KnownDim (Product d), Elt e, Fractional e) => Fractional (Tensor d e) where
  recip = liftF recip
  {-# INLINE recip #-}
  (/) = liftF2 (/)
  {-# INLINE (/) #-}
  fromRational = Tensor . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Product d))) . fromRational
  {-# INLINE fromRational #-}

instance (KnownDim (Product d), Elt e, Floating e) => Floating (Tensor d e) where
  pi = Tensor . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Product d))) $ pi
  {-# INLINE pi #-}
  exp = liftF exp
  {-# INLINE exp #-}
  sqrt = liftF sqrt
  {-# INLINE sqrt #-}
  log = liftF log
  {-# INLINE log #-}
  (**) = liftF2 (**)
  {-# INLINE (**) #-}
  logBase = liftF2 logBase
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
  :: forall d e. Elt e
  => KnownDim (Product d)
  => Vector e
  -> Maybe (Tensor d e)
constant v
  | V.length v == fromIntegral (dimVal (dim :: Dim (Product d))) = Just $ Tensor v
  | otherwise = Nothing

equal
  :: Elt e
  => Eq e
  => Tensor d e
  -> Tensor d e
  -> Tensor d Bool
equal = liftF2 (==)

notEqual
  :: Elt e
  => Eq e
  => Tensor d e
  -> Tensor d e
  -> Tensor d Bool
notEqual = liftF2 (/=)

less
  :: Elt e
  => Ord e
  => Tensor d e
  -> Tensor d e
  -> Tensor d Bool
less = liftF2 (<)

lessEqual
  :: Elt e
  => Ord e
  => Tensor d e
  -> Tensor d e
  -> Tensor d Bool
lessEqual = liftF2 (<=)

greater
  :: Elt e
  => Ord e
  => Tensor d e
  -> Tensor d e
  -> Tensor d Bool
greater = liftF2 (>)

greaterEqual
  :: Elt e
  => Ord e
  => Tensor d e
  -> Tensor d e
  -> Tensor d Bool
greaterEqual = liftF2 (>=)

maximum
  :: Elt e
  => Ord e
  => Tensor d e
  -> Tensor d e
  -> Tensor d e
maximum = liftF2 max

minimum
  :: Elt e
  => Ord e
  => Tensor d e
  -> Tensor d e
  -> Tensor d e
minimum = liftF2 min
