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
{-# LANGUAGE UndecidableInstances, RankNTypes, AllowAmbiguousTypes, PolyKinds  #-}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures#-} 
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}

module Numeric.Tensile.Operations.Linear.Internal where

import Data.Tensor.Types
import Data.Tensor.Internal.Array
import Data.Vector.Storable (Vector(..), Storable(..))
import Control.Monad.ST
import Control.Monad (void)


import Numeric.Dimensions (TypedList(..), Dimensions(..), Idx(..), Idxs(..), Dims(..), KnownDim(..), Reverse, Length)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M
import qualified Numeric.Dimensions as D

import Numeric.Type.Evidence

import Debug.Trace
import GHC.Base (unsafeCoerce#)


import Data.Monoid
import Data.Word


---------------- Data.Dimensions

import GHC.TypeNats
import qualified Math.Combinat.Permutations as P


instance Semigroup (Perm n) where
  (Perm p) <> (Perm q) = Perm $ P.multiply p q

instance KnownNat n => Monoid (Perm n) where
  mempty = Perm $ P.identity (fromIntegral $ natVal @n undefined)

newtype Perm (n :: Nat) = Perm P.Permutation deriving (Eq, Ord, Show)


transposition :: forall n. KnownDim n => Int -> Int -> Perm n
transposition i j = Perm $ P.transposition n' (i,j)
  where
    n' = fromIntegral $ D.dimVal' @n

transposition' 
  :: forall i j n. KnownNat n
  => KnownNat i
  => KnownNat j
  => i <= n
  => j <= n
  => Perm n
transposition'  = Perm $ P.transposition n (i,j)
  where
    n = fromIntegral $ natVal @n undefined
    i = fromIntegral $ natVal @i undefined
    j = fromIntegral $ natVal @j undefined

-----------------------

-- TODO
-- replace Vector t w/ Tensor t ds
-- relocate general utils

fromEnumD :: forall ds i. Integral i => Dims ds -> Idxs ds -> i
fromEnumD dims = fromIntegral . go 1 dims
  where
    go :: forall ns . Word -> Dims ns -> Idxs ns -> Word
    go _ U U                     = 0
    go m (d :* ds) (Idx i :* is) = m * (i - 1) + go (m * D.dimVal d) ds is


permuteIdx :: Perm (Length ds) -> Idxs ds -> Idxs ds
permuteIdx (Perm p) = unsafeCoerce# . P.permuteList p . D.listIdxs



constant :: Storable t => Dims (ds :: [Nat]) -> t -> Vector t
constant dims t = fill dims $ const t

fill :: Storable t => Dims (ds :: [Nat]) -> (Int -> t) -> Vector t
fill dims act = 
  case dims of 
    D.Reverse dims' -> fill' dims act
   
fill' :: Storable t => Dims (ds :: [Nat]) -> (Int -> t) -> Vector t
fill' dims act = V.create $ do
  v <- M.new (fromIntegral $ D.totalDim dims)
  let f i = M.write v i $ act i
  D.overDimOff_ dims f 0 1 -- either control the offset ourselves or fix type issues
  return v


{-

> fillIdx (dims @_ @'[2, 2, 3]) $ sum . listIdxs
[3,4,5,4,5,6,4,5,6,5,6,7]
> fillIdx' (dims @_ @'[2, 2, 3]) $ sum . listIdxs
[3,4,4,5,4,5,5,6,5,6,6,7]

overDimIdx_ d (\i -> print i >> print (fromEnumD d i) >> print (fromEnumD d $ permuteIdx tt i))

transpose :: forall t ds. (Storable t, Dimensions ds) => Dims ds ->  -> Vector t
transpose ds p v = modifyIdx ds act v
  where swap' v i i' = M.swap v (fromEnum i) (fromEnum i') 
        act i v = swap' i (p i) v
-}


fillIdx 
  :: forall t ds. Storable t
  => Dims ds -> (Idxs (Reverse ds) -> t) -> Vector t
fillIdx dims act = 
  case dims of 
    D.Reverse dims' -> fillIdx' dims' act

fillIdx' :: forall t ds. Storable t => Dims ds -> (Idxs ds -> t) -> Vector t
fillIdx' dims f = V.create $ do
  mv <- M.new (fromIntegral $ D.totalDim dims)
  let act idxs = M.write mv (fromEnumD dims idxs) $ f idxs
  D.overDimIdx_ dims act
  return mv

{-

--TODO Dimensions / Dims arg are redundant
fillIdx' :: forall t ds. Storable t => Dims ds -> (Dimensions ds => Idxs ds -> t) -> Vector t
fillIdx' ds f = V.create $ do
  mv <- M.new (fromIntegral $ D.totalDim ds)
  let act idxs = M.write mv (fromEnum idxs) $ f idxs
  D.overDimIdx_ ds act
  return mv


mod dims idxs m = M.swap m (fromEnumD dims idxs) (fromEnumD dims $ permuteIdx tt idxs)


TODO add tests:
> mod idxs m = M.swap m 0 (fromEnum idxs)
> v = V.fromList [1..12]
> v
[1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0]
> modifyIdx (dims @_ @'[2, 2, 3]) mod v
[12.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0]

> modifyIdx (dims @_ @'[2, 2, 3]) (\_ m -> M.set m 0) v
[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
-}

--TODO Dimensions / Dims arg are redundant
modifyIdx :: forall t ds. Storable t => Dims ds -> (forall s. Idxs ds -> M.MVector s t -> ST s ()) -> Vector t -> Vector t
modifyIdx dims f = V.modify $ \mv -> do
  let act i = f i mv
  D.overDimIdx_ dims act

t :: Perm 12
t = transposition 3 4

tt :: Perm 2
tt = transposition' @1 @2

modd
  :: (Storable a, Length ds ~ 2) =>
     Dims (ds :: [Nat])
     -> Idxs ds
     -> V.MVector s a 
     -> ST s ()
modd dims idxs m = M.swap m (fromEnumD dims idxs) (fromEnumD dims $ permuteIdx tt idxs)

modifyIdx' :: forall t (ds :: [Nat]) . Storable t => Dims ds -> (forall s.  Dims (Reverse ds) -> Idxs (Reverse ds) -> M.MVector s t -> ST s ()) -> Vector t -> Vector t
modifyIdx' dims f = 
  case dims of 
    D.Reverse dims' -> V.modify mod
      where
        mod :: forall s. M.MVector s t -> ST s () 
        mod mv = D.overDimIdx_ dims' $ \i -> f dims' i mv

v :: Tensor Float '[3, 4]
v = Tensor $ V.fromList [1..15]

out :: Tensor Float '[4, 3]
out = Tensor $ modifyIdx' (dims @_ @'[3,4]) modd (unTensor v)
{-

modify :: Storable a => (forall s. MVector s a -> ST s ()) -> Vector a -> Vector a
swap :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> Int -> m () Source#


foldIdx = undefined

foldIdx' :: forall t ds. (Storable t, Dimensions ds) => Dims ds -> (Idxs ds -> t -> t) -> Vector t 
foldIdx' dims act = V.create $ do
  v <- M.new (fromIntegral $ D.totalDim dims)
  let f i = M.write v (fromEnum i) $ act i
  D.overDimIdx_ dims f
  return v

overDimIdx_ :: Monad m
            => Dims ds -- ^ Shape of a space
            -> (Idxs ds -> m ()) -- ^ Function to call on each dimension
            -> m ()

overDimIdx :: Monad m
           => Dims ds -- ^ Shape of a space
           -> (Idxs ds -> a -> m a) -- ^ Function to call on each dimension
           -> a -- ^ Initial value
           -> m a


-- | Fold over all dimensions keeping track of index
foldDimIdx :: Dims ds -- ^ Shape of a space
           -> (Idxs ds -> a -> a) -- ^ Function to call on each dimension
           -> a -- ^ Initial value
           -> a


-- Use for matmul??
--
--
-- | Traverse from the first index to the second index in each dimension.
--   You can combine positive and negative traversal directions
--   along different dimensions.
--
--   Note, initial and final indices are included in the range;
--   the argument function is guaranteed to execute at least once.
overDimPartIdx :: Monad m
               => Idxs ds -- ^ Initial indices
               -> Idxs ds -- ^ Final indices
               -> (Idxs ds -> a -> m a)
                          -- ^ Function to call on each dimension
               -> a       -- ^ initial value
               -> m a
overDimPartIdx U U k = k U


Swap the elements at the given positions.



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


