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
import Control.Monad (void, unless)


import Numeric.Dimensions (TypedList(..), Dimensions(..), Idx(..), Idxs(..), Dims(..), KnownDim(..), Reverse, Length)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M
import qualified Numeric.Dimensions as D hiding (idxsFromWords) -- broken function

import Numeric.Type.Evidence

import Debug.Trace
import GHC.Base (unsafeCoerce#)


import Data.Monoid
import Data.Word

import Data.List (sort)
---------------- Data.Dimensions

import GHC.TypeNats
import qualified Math.Combinat.Permutations as P

-- Numeric.Tensile.Dimensions
type family Order (xs :: [k]) :: Nat where
    Order '[] = 0
    Order (_ ': xs) = 1 + Order xs

type family Size (xs :: [k]) :: Nat where
    Size '[] = 1
    Size (x ': xs) = x * Size xs

-- TODO : fix in the upstream lib
idxsFromWords :: forall ds . Dimensions ds => [Word] -> Maybe (Idxs ds)
idxsFromWords = unsafeCoerce# . go (D.listDims (D.dims @_ @ds))
  where
    go [] [] = Just []
    go (d : ds) (i : is)
      | i > 0 && i <= d = (i:) <$> go ds is
    go _ _   = Nothing

foldDimPartIdx :: Idxs ds -- ^ Initial indices
               -> Idxs ds -- ^ Final indices
               -> (Idxs ds -> a -> a)
                          -- ^ Function to call on each dimension
               -> a       -- ^ initial value
               -> a
foldDimPartIdx U U k = k U
foldDimPartIdx (start :* starts) (end :* ends) k
  | iEnd >= iStart = foldDimPartIdx starts ends (loop iStart)
  | otherwise      = foldDimPartIdx starts ends (looi iStart)
  where
    Idx iStart = start
    Idx iEnd   = end
    loop i is
      | i > iEnd = id
      | otherwise = k (Idx i :* is) . loop (i+1) is
    looi i is
      | i < iEnd = id
      | otherwise = k (Idx i :* is) . looi (i-1) is

overDimPartIdx_ :: Monad m
               => Idxs ds -- ^ Initial indices
               -> Idxs ds -- ^ Final indices
               -> (Idxs ds -> m ())
                          -- ^ Function to call on each dimension
               -> m ()
overDimPartIdx_ U U k = k U
overDimPartIdx_ (start :* starts) (end :* ends) k
  | iEnd >= iStart = overDimPartIdx_ starts ends loop'
  | otherwise      = overDimPartIdx_ starts ends looi'
  where
    Idx iStart = start
    Idx iEnd   = end
    loop' is = loop iStart
      where
        loop i
          | i > iEnd = return ()
          | otherwise = k (Idx i :* is) >> loop (i+1)
    looi' is = looi iStart
      where
        looi i
          | i < iEnd = return ()
          | otherwise = k (Idx i :* is) >> looi (i-1)



-- Numeric.Tensile.Permutation
instance Semigroup (Perm n) where
  (Perm p) <> (Perm q) = Perm $ P.multiply p q

instance KnownNat n => Monoid (Perm n) where
  mempty = Perm $ P.identity (fromIntegral $ natVal @n undefined)

newtype Perm (n :: Nat) = Perm { unPerm :: P.Permutation } deriving (Eq, Ord, Show)


transposition :: forall n. KnownDim n => Int -> Int -> Perm n
transposition i j = Perm $ P.transposition n' (i,j)
  where
    n' = fromIntegral $ D.dimVal' @n

transpositionP :: forall n. KnownDim n => Int -> Int -> Perm n
transpositionP i j = Perm $ P.transposition n' (i+1,j+1)
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

transposition'' 
  :: forall i j n. KnownNat n
  => KnownNat i
  => KnownNat j
  => i <= n
  => j <= n
  => Perm n
transposition''  = Perm $ P.transposition n (i+1,j+1)
  where
    n = fromIntegral $ natVal @n undefined
    i = fromIntegral $ natVal @i undefined
    j = fromIntegral $ natVal @j undefined

-----------------------

-- | Transform a permutation of tensor modes into a permutation of array indices.
-- permute (lowerPerm p1) . permute (lowerPerm p2) == permute (lowerPerm $ p1 <> p2)
lowerPerm 
  :: forall ds. KnownNat (Size ds) 
  => Dims ds 
  -> Perm (Order ds) -- ^ Order-level permutation
  -> (Dims ds -> Idxs ds -> Perm (Order ds) -> Perm (Size ds)) -- ^ Index filter
  -> Perm (Size ds)  -- ^ Index-level permutation
lowerPerm d p f = D.foldDimIdx d (\i p' -> p' <> f d i p) (mempty :: Perm (Size ds))



-- minorToMajor = permute (lowerPerm reversal)
-- see https://www.tensorflow.org/xla/shapes#minor-to-major_dimension_ordering

reversal :: forall n. KnownNat n => Perm n
reversal = Perm $ P.reversePermutation n
  where n = fromIntegral $ natVal @n undefined


-- Unsafe use w/ care
permuteIdxs :: Perm (Order ds) -> Idxs ds -> Idxs ds
permuteIdxs (Perm p) = unsafeCoerce# . P.permuteList p . D.listIdxs

{-
 - > d' = (dims @_ @'[2,3,4])
> permuteDims ttt d'
Dims [2,4,3]
> :t permuteDims ttt d'
permuteDims ttt d' :: Dims '[2, 3, 4]
-}
-- Unsafe use w/ care
permuteDims :: Perm (Order ds) -> Dims ds -> Dims ds
permuteDims (Perm p) = unsafeCoerce# . P.permuteList p . D.listDims

-- Unsafe use w/ care
reverseDims :: Dims ds -> Dims ds
reverseDims = unsafeCoerce# . reverse . D.listDims

reverseIdxs :: Idxs ds -> Idxs ds
reverseIdxs = unsafeCoerce# . reverse . D.listIdxs

-- TODO
-- replace Vector t w/ Tensor t ds
-- relocate general utils
-- consider undoing index offset here
majorToMinor :: forall ds i. Integral i => Dims ds -> Idxs ds -> i
majorToMinor dims = fromIntegral . go 1 dims
  where
    go :: forall ns . Word -> Dims ns -> Idxs ns -> Word
    go _ U U                     = 0
    go m (d :* ds) (Idx i :* is) = m * (i - 1) + go (m * D.dimVal d) ds is


-- NOTE unless you're using enum you dont need to reverse idxs

minorToMajor :: forall ds i. Integral i => Dims ds -> Idxs ds -> i
minorToMajor d i = majorToMinor (reverseDims d) (reverseIdxs i)

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


> fillIdx' (dims @_ @'[2, 3, 3]) $ sum . listIdxs
[3,4,4,5,5,6,4,5,5,6,6,7,5,6,6,7,7,8]

> reversed d' $ sum . listIdxs
[3,4,5,4,5,6,5,6,7,4,5,6,5,6,7,6,7,8]
> reversed' d' $ sum . listIdxs
[3,4,5,4,5,6,5,6,7,4,5,6,5,6,7,6,7,8]


> V.map (+1) $ reversed d' (majorToMinor d') :: Vector Int
[1,7,13,3,9,15,5,11,17,2,8,14,4,10,16,6,12,18]

V.map (+1) $ shuffled e d' (majorToMinor d') :: Vector Int


fillIdx d' $ sum . listIdxs

[1,7,13,3,9,15,5,11,17,2,8,14,4,10,16,6,12,18]

-}

-- TODO use this pattern
reversed :: forall t ds. (Storable t, KnownNat (Order ds)) => Dims ds -> (Idxs ds -> t) -> Vector t
reversed = shuffled (reversal :: Perm (Order ds))

shuffled :: forall t ds. (Storable t) => Perm (Order ds) -> Dims ds -> (Idxs ds -> t) -> Vector t
shuffled p ds f = fillIdx' (permuteDims p ds) $ f . permuteIdxs p

fillIdx 
  :: forall t ds. Storable t
  => Dims ds -> (Idxs (Reverse ds) -> t) -> Vector t
fillIdx dims act = 
  case dims of 
    D.Reverse dims' -> fillIdx' dims' act




fillIdx' :: forall t ds. Storable t => Dims ds -> (Idxs ds -> t) -> Vector t
fillIdx' dims f = V.create $ do
  mv <- M.new (fromIntegral $ D.totalDim dims)
  let act idxs = M.write mv (majorToMinor dims idxs) $ f idxs
  D.overDimIdx_ dims act
  return mv

fillIdx'' :: forall t ds. Storable t => Dims ds -> (Idxs ds -> t) -> Vector t
fillIdx'' dims f = V.create $ do
  mv <- M.new (fromIntegral $ D.totalDim dims)
  let act idxs = M.write mv (minorToMajor dims idxs) $ f idxs
  D.overDimIdx_ dims act
  return mv

{-

mod dims idxs m = M.swap m (majorToMinor dims idxs) (majorToMinor dims $ permuteIdxs tt idxs)


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

modifyIdx :: forall t ds. Storable t => Dims ds -> (forall s. Idxs ds -> M.MVector s t -> ST s ()) -> Vector t -> Vector t
modifyIdx dims f = V.modify $ \mv -> do
  let act i = traceShow i $ f i mv
  D.overDimIdx_ dims act





{-
--typesafe version
modifyIdx' :: forall t ds . Storable t => Dims ds -> (forall s. Dims (Reverse ds) -> Idxs (Reverse ds) -> M.MVector s t -> ST s ()) -> Vector t -> Vector t
modifyIdx' dims f = 
  case dims of 
    D.Reverse dims' -> V.modify mod
      where
        mod :: forall s. M.MVector s t -> ST s () 
        mod mv = D.overDimIdx_ dims' $ \i -> f dims' i mv

-}




cycles (Perm t) = P.permutationToDisjointCycles $ t


triangular :: forall ds. KnownNat (Size ds) => Dims ds -> Idxs ds -> Perm (Order ds) -> Perm (Size ds)
triangular d idxs p = if ord idxs then p' else (mempty :: Perm (Size ds))
  where
    p' = Perm $ P.transposition td ((majorToMinor d idxs)+1,(majorToMinor d $ permuteIdxs p idxs)+1)
    ord idxs = sort (D.listIdxs idxs) == D.listIdxs idxs
    td = fromIntegral $ D.totalDim d

filterIdx 
  :: (Idxs ds -> Bool)
  -> Perm (Order ds) 
  -> Idxs ds 
  -> Idxs ds
filterIdx c p i = if c i then i' else i where i' = permuteIdxs p i
   


debug :: Dims ds -> Perm (Order ds) -> (Perm (Order ds) -> Idxs ds -> Idxs ds) -> IO ()
debug d p f = D.overDimIdx_ d $ \i -> 
  unless (i==f p i) (print (1 + minorToMajor d i, 1 + minorToMajor d (f p i))) 


d' = (dims @_ @'[2,3,3])

e :: Perm 3
e = mempty

rev :: Perm 3
rev = reversal 

ttt :: Perm 3
ttt = transposition' @2 @3

triangular' :: Idxs ds -> Bool
triangular' i = sort (D.listIdxs i) == D.listIdxs i

triangular'' :: Idxs ds -> Bool
triangular'' i = sort (tail $ D.listIdxs i) == tail (D.listIdxs i)

mod'' :: forall t ds s. Storable t => (Idxs ds -> Idxs ds) -> Dims ds -> Idxs ds -> M.MVector s t -> ST s ()
mod'' p d i m = M.swap m (minorToMajor d (p i)) (minorToMajor d i)

w = V.map (+1) $ fillIdx'' d' $ majorToMinor d' :: Vector Int -- col major
w' = V.map (+1) $ fillIdx'' d' $ minorToMajor d' :: Vector Int -- row major

{- unit tests
 -
 -

--test 0

x <- gen

res = modifyIdx d' (mod'' (filterIdx triangular'' e) d') x

res == x

--test 1:

w = V.map (+1) $ fillIdx'' d' $ majorToMinor d' :: Vector Int -- col major
w' = V.map (+1) $ fillIdx'' d' $ minorToMajor d' :: Vector Int -- row major
> w
[1,7,13,3,9,15,5,11,17,2,8,14,4,10,16,6,12,18]
> w'
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18]

modifyIdx d' (mod'' p' d') w'

modifyIdx d' (mod'' (filterIdx triangular' rev) d') w'


--test 2:

res = modifyIdx d' (mod'' (filterIdx triangular'' ttt) d') w'
res == [1,4,7,2,5,8,3,6,9,10,13,16,11,14,17,12,15,18]


--test 3:
a :: Maybe (Idxs '[2, 3, 3])
a = idxsFromWords [2, 1, 1]  

b :: Maybe (Idxs '[2, 3, 3])
b = idxsFromWords [2, 3, 3] 

check :: Dims ds -> Idxs ds -> Idxs ds -> Perm (Order ds) -> (Perm (Order ds) -> Idxs ds -> Idxs ds) -> [(Int, Int)]
check d i j p f = foldDimPartIdx i j acc []
  where 
    acc i l = if (i/=f p i) then (1 + minorToMajor d i, 1 + minorToMajor d (f p i)) : l else l

res = liftM2 (\i j -> check d' i j ttt $ filterIdx triangular'') a b

res == Just [(11,13),(12,16),(15,17)]



-}




{-

:: Maybe (Idxs '[2, 3, 3])

d' = (dims @_ @'[2,3,3])

ttt :: Perm 3
ttt = transposition' @2 @3

p :: Idxs '[2, 3, 3] -> Idxs '[2, 3, 3]
--p :: (Order ds ~ 3) => Idxs ds -> Idxs ds
p = filterIdx triangular'' ttt

> modifyIdx d' (mod'' p d') w'
[1,4,7,2,5,8,3,6,9,10,13,16,11,14,17,12,15,18]


modifyIdx d' (mod'' (filterIdx triangular'' ttt) d') w'

debug d' ttt $ filterIdx triangular''

fromJust $ liftM2 (\i j -> debug' d' i j ttt $ filterIdx triangular'') a b


> lowerPerm d' ttt triangular
Perm {unPerm = toPermutation [1,2,7,4,13,6,3,8,9,10,15,16,5,14,11,12,17,18]}

> lowerPerm d' ttt' triangular
Perm {unPerm = toPermutation [1,2,3,4,5,6,7,9,8,10,11,12,13,15,17,16,18,14]}

> lowerPerm d (reversal :: Perm 2) triangular
Perm (toPermutation [1,4,7,2,5,8,3,6,9])

p = D.foldDimIdx d' (\i p -> p <> triangular d' i ttt) mempty
> p
> foo p
DisjointCycles [[2,4],[3,7],[6,8]]





modify :: Storable a => (forall s. MVector s a -> ST s ()) -> Vector a -> Vector a
swap :: (PrimMonad m, Storable a) => MVector (PrimState m) a -> Int -> Int -> m () Source#


foldIdx = undefined


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


