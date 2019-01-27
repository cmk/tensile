{-# LANGUAGE FlexibleInstances, KindSignatures, MagicHash, TypeOperators, UnboxedSums, UnboxedTuples, UndecidableInstances #-}

module Data.Tensor.Internal.Array where

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

-- | Broadcast element into array
broadcast :: PrimBytes t => t -> ArrayBase t ds
broadcast t = ArrayBase (# t | #)
{-# INLINE broadcast #-}

-- | Index an array given an offset
ix# :: PrimBytes t => Int# -> ArrayBase t ds -> t
ix# i (ArrayBase a) = case a of
  (# t | #)                 -> t
  (# | (# off, _, arr #) #) -> indexArray arr (off +# i)
{-# INLINE ix# #-}

-- | Generate an array using an accumulator funtion
gen# 
  :: forall s t ds. PrimBytes t 
  => Int# -- ^ number of elements, not checked!
             --   Avoid using this argument if possible.
  -> (s -> (# s, t #))
  -> s -> (# s, ArrayBase t ds #)
gen# n f z0 = go (byteSize @t undefined *# n)
  where
    go bsize = case runRW#
     ( \s0 -> case newByteArray# bsize s0 of
         (# s1, mba #) -> case loop0 mba 0# z0 s1 of
           (# s2, z1 #) -> case unsafeFreezeByteArray# mba s2 of
             (# s3, ba #) -> (# s3, (# z1, ba #) #)
     ) of (# _, (# z1, ba #) #) -> (# z1, ArrayBase (# | (# 0# , n , ba #) #) #)
    {-# NOINLINE go #-}
    loop0 mba i z s
      | isTrue# (i ==# n) = (# s, z #)
      | otherwise = case f z of
          (# z', x #) -> loop0 mba (i +# 1#) z' (writeArray mba i x s)
{-# INLINE gen# #-}

-- | update a single element in an array given an offset
upd# :: PrimBytes t => Int# -> Int# -> t -> ArrayBase t ds -> ArrayBase t ds
upd# n i x (ArrayBase (# a | #)) = go (byteSize x)
  where
    go tbs = case runRW#
     ( \s0 -> case newByteArray# (tbs *# n) s0 of
         (# s1, mba #) -> unsafeFreezeByteArray# mba
           (writeArray mba i x
             (loop1# n (\j -> writeArray mba j a) s1)
           )
     ) of (# _, r #) -> ArrayBase (# | (# 0# , n , r #) #)
    {-# NOINLINE go #-}
upd# _ i x (ArrayBase (# | (# offN , n , ba #) #)) = go (byteSize x)
  where
    go tbs = case runRW#
     ( \s0 -> case newByteArray# (tbs *# n) s0 of
         (# s1, mba #) -> unsafeFreezeByteArray# mba
           (writeArray mba i x
             (copyByteArray# ba (offN *# tbs) mba 0# (tbs *# n) s1)
           )
     ) of (# _, r #) -> ArrayBase (# | (# 0# , n , r #) #)
    {-# NOINLINE go #-}
{-# INLINE upd# #-}

-- | Offset of an array in number of elements
elemOffset :: PrimBytes t => ArrayBase t ds -> Int#
elemOffset (ArrayBase a) = case a of
  (# _ | #)               -> 0#
  (# | (# off, _, _ #) #) -> off
{-# INLINE elemOffset #-}

-- | Number of elements in an array.
--   Returns zero if this information is not available at runtime.
--   This is possible only if all elements are same in an array.
elemSize0 :: PrimBytes t => ArrayBase t ds -> Int# 
elemSize0 (ArrayBase a) = case a of
  (# _ | #)             -> 0#
  (# | (# _, n, _ #) #) -> n
{-# INLINE elemSize0 #-}

fromElems ::  PrimBytes t => Int# -> Int# -> ByteArray# -> ArrayBase t ds
fromElems off n ba = ArrayBase (# | (# off , n , ba #) #)
{-# INLINE fromElems #-}


{-

broadcast ::  

ix# :: Int# -> ArrayBase t ds -> t 

gen# :: Int# -> (s -> (#s, t#)) -> s -> (#s, ArrayBase t ds#) 













elemSize0 :: a -> Int#

-- | Get array by its offset and size in a ByteArray.
--   Both offset and size are given in element number.
fromElems :: Int# -> Int# -> ByteArray# -> a

-}
