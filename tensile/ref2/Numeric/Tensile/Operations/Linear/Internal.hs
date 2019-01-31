{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UnboxedTuples          #-}
{-# LANGUAGE UndecidableInstances, RankNTypes   #-}

module Numeric.Tensile.Operations.Linear.Internal where

import Data.Tensor.Types hiding (constant)
import Data.Tensor.Internal.Array
import Data.Vector.Storable (Vector(..), Storable(..))
import Control.Monad.ST
import Control.Monad

import GHC.Base (unsafeCoerce#)
import GHC.TypeLits
import Numeric.Dimensions (Dimensions(..), Dims, Idxs, Reverse)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M
import qualified Numeric.Dimensions as D



--TODO: don't specialize to [Nat]
--      figure out how to reverse these lists!
--
reverseIdxs :: Idxs (ds :: [Nat]) -> Idxs (Reverse ds)
reverseIdxs dims = unsafeCoerce# (reverse (unsafeCoerce# dims))
{-# INLINE reverseIdxs #-}

reverseDims :: Dims (ds :: [Nat]) -> Dims (Reverse ds)
reverseDims dims = unsafeCoerce# (reverse (unsafeCoerce# dims))
{-# INLINE reverseDims #-}

constant :: Storable t => Dims (ds :: [Nat]) -> t -> Vector t
constant dims t = fill dims $ const t

fill :: Storable t => Dims (ds :: [Nat]) -> (Int -> t) -> Vector t
fill dims act = V.create $ do
  v <- M.new (fromIntegral $ D.totalDim dims)
  let f i = M.write v i $ act i
  D.overDimOff_ dims f 0 1
  return v

fill' :: (Storable t, Dimensions ds) => Dims (ds :: [Nat]) -> (Idxs ds -> t) -> Vector t
fill' dims act = V.create $ do
  v <- M.new (fromIntegral $ D.totalDim dims)
  let f i = M.write v (fromEnum i) $ act i
  D.overDimIdx_ dims f
  return v

{-
 -
fill (dims @_ @'[2, 2, 3]) id
fill' (dims @_ @'[2, 2, 3]) $ sum . listIdxs

fill' :: (Storable t, Dimensions ds) => Dims (ds :: [Nat]) -> (Idxs ds -> t) -> Vector t
fill' dims act = V.create $ do
  v <- M.new (fromIntegral $ D.totalDim dims)
  let f i = M.write v (fromEnum (reverseIdxs i)) $ act i
      dims' = reverseDims dims
  D.overDimIdx_ dims' f
  return v


t dims f = Tensor $ V.create $ st dims f

overDimIdx_ :: Monad m	=> Dims ds-> (Idxs ds -> m ()) -> m ()


Function to call on each dimension

write = 
overDimOff_ M.write 

overDimOff_ Source#

:: Monad m	 
=> Dims ds	
Shape of a space
-> (Int -> m ())	
Function to call with each offset value
-> Int	
Initial offset
-> Int	
Offset step
-> m ()

constant
  :: forall s t ds. Storable t
  => Dims ds
  -> t
  -> Tensor t ds
constant dims t = Tensor $ V.create st
  where
    st :: ST s (M.MVector s t)
    st = M.new (fromIntegral $ totalDim dims) >>= overDim dims f 0 1

    f :: Idxs ds -> Int -> (M.MVector s t) -> ST s (M.MVector s t)
    f _ i v = M.write v i t >> return v --M.replicate n t


--
M.new :: (PrimMonad m, Storable a) => Int -> m (MVector (PrimState m) a)
M.write :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> a -> m ()

create :: Storable a => (forall s. ST s (MVector s a)) -> Vector a



replicate :: (PrimMonad m, Storable a) => Int -> a -> m (MVector (PrimState m) a) Source#


Int -> m (MVector (PrimState m) a)

          ( overDim_# dbs
            ( \i pos ->
                writeArray mba pos (f i (indexOffset# (pos *# lenAS') lenAS' df))
            ) 0# 1# s1

overDim_# :: Dims (ds :: [k])
          -> (Idxs ds -> Int# -> State# s -> State# s) -- ^ function to map over each dimension
          -> Int# -- ^ Initial offset
          -> Int# -- ^ offset step
          -> State# s
          -> State# s

overDim_'# :: Dims (ds :: [k])
           -> (Idxs ds -> Int# -> State# s -> (# State# s, Int# #)) -- ^ function to map over each dimension
           -> Int# -- ^ Initial offset
           -> State# s
           -> (# State# s, Int# #)

overDimOff_ :: Monad m
            => Dims ds -- ^ Shape of a space
            -> (Int -> m ()) -- ^ Function to call with each offset value
            -> Int -- ^ Initial offset
            -> Int -- ^ Offset step
            -> m ()

overDim :: Monad m
        => Dims ds -- ^ Shape of a space
        -> (Idxs ds -> Int -> a -> m a) -- ^ Function to call on each dimension
        -> Int -- ^ Initial offset
        -> Int -- ^ Offset step
        -> a -- ^ Initial value
        -> m a



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

3-D tensor `a` w shape=[2, 2, 3]
[[[ 1,  2,  3],
  [ 4,  5,  6]],
 [[ 7,  8,  9],
  [10, 11, 12]]]


3-D tensor `b` w shape=[2, 3, 2]
[[[13, 14],
  [15, 16],
  [17, 18]],
 [[19, 20],
  [21, 22],
  [23, 24]]]
                )

mul `a` `b` has shape=[2,2,2]
[[[ 94, 100],
  [229, 244]],
 [[508, 532],
  [697, 730]]]
-}


{-
product 
  :: forall m x y. KnownDim m
  => Dimensions x
  => Dimensions y
  => T (x +: m) -> T (m :+ y) -> T (x ++ y)
product t u
    | I# m <- fromIntegral $ dimVal' @m
    , I# n <- fromIntegral $ totalDim' @x
    , I# k <- fromIntegral $ totalDim' @y
    , nk <- n *# k
    = let loop1 i j l r | isTrue# (l ==# m) = r
                        | otherwise = loop1 i j (l +# 1#)
                          (r + ix# (i +# n *# l) t * ix# (l +# m *# j) u)

          loop2 (T# i j) | isTrue# (j ==# k) = (# T# i j, 0 #)
                         | isTrue# (i ==# n) = loop2 (T# 0# (j +# 1#))
                         | otherwise = (# T# (i +# 1#) j, loop1 i j 0# 0 #)
      in case gen# nk loop2 (T# 0# 0#) of
          (# _, r #) -> r

data T# = T# Int# Int#

-- mode-i tensor-matrix product
-- see http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=4A0663C7848627DADDBA6A243BC43E78?doi=10.1.1.130.782&rep=rep1&type=pdf
product
  :: forall m x y. KnownDim m
  => Dimensions x
  => Dimensions y
  => T (x ++ [m] ++ y) 
  -> T '[m, n]
  -> T (x ++ n ++ y)

-- #>
productR
  :: All KnownDim '[a, b, c]
  => Dimensions x
  => T (a :+ b :+ x)
  -> T (b :+ c :+ x)
  -> T (a :+ c :+ x)
productR = undefined

-- <#
-- same as tf.matmul
productN
  :: All KnownDim '[a, b, c]
  => Dimensions x
  => T (x +: a +: b) 
  -> T (x +: b +: c)
  -> T (x +: a +: c)
productN = undefined


transpose
  :: forall m n x. All KnownDim '[m, n]
  => T '[n, m] --(n :+ m :+ x) 
  -> T '[m, n] --(m :+ n :+ x)
transpose t = case elemSize0 t of
  0# -> broadcast (ix# 0# t)
  nm | I# m <- fromIntegral $ dimVal' @m
     , I# n <- fromIntegral $ dimVal' @n
     -> let f ( I# i,  I# j )
              | isTrue# (i ==# m) = f ( 0 , I# (j +# 1#) ) -- skip to next col
              | otherwise         = (# ( I# (i +# 1#), I# j ), ix# (i *# n +# j) t #) --col-major indexing
        in case gen# nm f (0,0) of
          (# _, r #) -> r


-}


