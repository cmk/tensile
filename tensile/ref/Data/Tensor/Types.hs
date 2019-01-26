{-# LANGUAGE FlexibleInstances, KindSignatures, MagicHash, TypeOperators, UnboxedSums, UnboxedTuples, UndecidableInstances #-}

module Data.Tensor.Types where

import Data.Bits
import Data.Singletons.Prelude.List (Product)
import Data.Word (Word8)
import GHC.Base hiding (foldr)
import GHC.TypeLits
import Numeric.Dimensions --(Dimensions(..), KnownDim(..), dimVal)
import Numeric.DataFrame
import Numeric.DataFrame.Internal.Array.Family.ArrayBase
import Numeric.DataFrame.Internal.Array.Class
import Numeric.DataFrame.Internal.Array.PrimOps

import Numeric.PrimBytes

import qualified Data.Vector.Storable as V

-- TODO: move to application / test stanza
type TVal = Float
type IVal = Int
type BVal = Word8

-- TODO fix, keep in mind you need BOOLs
type Elt = PrimBytes
--class Elt e
type Tensor = ArrayBase

-- | A real or complex-valued tensor of shape 'd'. 
type T d = Tensor TVal d

-- | An integer or non-negative integer-valued tensor of shape 'd'. 
type I d = Tensor IVal d

-- | A boolean-valued tensor of shape 'd'. 
type B d = Tensor BVal d



{-



instance (KnownDim (Product d), Elt e, Eq e, Bits e, Num e) => Bits (Tensor e d) where
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
-}
{-

instance (KnownDim (Product d), Elt e, Real e) => Real (Tensor e d) where
  toRational = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

instance (KnownDim (Product d), Elt e, Enum e) => Enum (Tensor e d) where
  toEnum = Tensor . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Product d))) . toEnum
  {-# INLINE toEnum #-}
  fromEnum = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

instance (KnownDim (Product d), Elt e, Integral e) => Integral (Tensor e d) where
  quot (Tensor a) (Tensor b) = liftF2 quot a b
  rem (Tensor a) (Tensor b) = liftF2 rem a b
  div (Tensor a) (Tensor b) = liftF2 div a b
  mod (Tensor a) (Tensor b) = liftF2 mod a b
  quotRem ta tb = (quot ta tb, rem ta tb)
  divMod ta tb = (div ta tb, mod ta tb)
  toInteger _ = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

-}



{-

constant
  :: forall d e. Elt e
  => KnownDim (Product d)
  => V.Vector e
  -> Maybe (Tensor e d)
constant v
  | V.length v == fromIntegral (dimVal (dim :: Dim (Product d))) = Just $ Tensor v
  | otherwise = Nothing

equal
  :: Elt e
  => Eq e
  => Tensor e d
  -> Tensor e d
  -> Tensor BVal d
equal = liftF2 (==)

notEqual
  :: Elt e
  => Eq e
  => Tensor e d
  -> Tensor e d
  -> Tensor BVal d
notEqual = liftF2 (/=)

less
  :: Elt e
  => Ord e
  => Tensor e d
  -> Tensor e d
  -> Tensor BVal d
less = liftF2 (<)

lessEqual
  :: Elt e
  => Ord e
  => Tensor e d
  -> Tensor e d
  -> Tensor BVal d
lessEqual = liftF2 (<=)

greater
  :: Elt e
  => Ord e
  => Tensor e d
  -> Tensor e d
  -> Tensor BVal d
greater = liftF2 (>)

greaterEqual
  :: Elt e
  => Ord e
  => Tensor e d
  -> Tensor e d
  -> Tensor BVal d
greaterEqual = liftF2 (>=)
-}


maximum
  :: Elt e
  => Ord e
  => Tensor e d
  -> Tensor e d
  -> Tensor e d
maximum = liftF2 max

minimum
  :: Elt e
  => Ord e
  => Tensor e d
  -> Tensor e d
  -> Tensor e d
minimum = liftF2 min

--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------


ix :: (PrimBytes t, Dimensions ds) => Idxs ds -> ArrayBase t ds -> t
ix i (ArrayBase a) = case a of
  (# t | #)  -> t
  (# | (# off, _, arr #) #) -> case fromEnum i of
    I# i# -> indexArray arr (off +# i#)
{-# INLINE ix #-}

undefEl :: ArrayBase t ds -> t
undefEl = const undefined

liftF :: PrimBytes t => (t -> t) -> ArrayBase t ds -> ArrayBase t ds
liftF f (ArrayBase (# t | #))
    = ArrayBase (# f t | #)
liftF f x@(ArrayBase (# | (# offN, n, ba #) #))
    | tbs <- byteSize (undefEl x)
    = go (tbs *# n)
  where
    go bsize = case runRW#
     ( \s0 -> case newByteArray# bsize s0 of
         (# s1, mba #) -> unsafeFreezeByteArray# mba
           ( loop1# n
               (\i -> writeArray mba i (f (indexArray ba (offN +# i)))) s1
           )
     ) of (# _, r #) -> ArrayBase (# | (# 0#, n, r #) #)
    {-# NOINLINE go #-}
{-# INLINE liftF #-}

liftF2 :: PrimBytes t => (t -> t -> t)
     -> ArrayBase t ds -> ArrayBase t ds -> ArrayBase t ds
liftF2 f (ArrayBase (# x | #)) b = liftF (f x) b
liftF2 f a (ArrayBase (# y | #)) = liftF (flip f y) a
liftF2 f a@(ArrayBase (# | (# oa, na, ba #) #))
         (ArrayBase (# | (# ob, nb, bb #) #))
    | n <- (minInt# na nb)
    = go n (byteSize (undefEl a) *# n)
  where
    go n bsize = case runRW#
     ( \s0 -> case newByteArray# bsize s0 of
         (# s1, mba #) -> unsafeFreezeByteArray# mba
           ( loop1# n
               (\i -> writeArray mba i
                        (f (indexArray ba (oa +# i))
                           (indexArray bb (ob +# i))
                        )
               ) s1
           )
     ) of (# _, r #) -> ArrayBase (# | (# 0#, n, r #) #)
    {-# NOINLINE go #-}
{-# INLINE liftF2 #-}
