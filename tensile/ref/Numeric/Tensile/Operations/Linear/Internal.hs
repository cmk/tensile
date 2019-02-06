{-
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
-}

module Numeric.Tensile.Operations.Linear.Internal where

import Data.Tensor.Types
import Data.Tensor.Internal.Array
import Data.Vector.Storable (Vector(..), Storable(..))
import Control.Monad.ST (ST(..))
import Numeric.Tensile.Types
import Numeric.Tensile.Index
import Numeric.Tensile.Permutation
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M


-- import Data.Word



{-
lowerPerm'
  :: forall ds. KnownDim (Size ds) 
  => (Dims ds' -> Idxs ds' -> Perm (Rank ds) -> Perm (Rank ds))
  -> Dims ds 
  -> Perm (Rank ds) -- ^ Rank-level permutation
  -> Perm (Rank ds)  -- ^ Index-level permutation
lowerPerm' d p f = foldDimIdx d (\i p' -> p' <> f d i p) (mempty :: Perm (Size ds))
-}
-- f :: (Dims ds' -> Perm n -> Perm n) -> Perm n -> Tensor t ds -> Tensor t ds'
-- f dim2Idx perm t = Tensor $ reifyDims (permuteDims perm (dims @_ @ds)) $ \p ->
--   modifyIdx (reflect p) (modify (permuteIdxs (dim2Idx (reflect p) perm) _)) (reflect p)) t -- basically make user derive the Idxs ds' -> Idxs ds'

-- dim2Idx :: Rank ds ~ n => Dims ds -> Perm n -> Perm n
-- takes a perm on dimensions and derives a perm in indices, eg
-- dim2Idx d p = lowerPerm' ...
-- --
-- otherwise consider using the raw index folds and lowerPerm???
-- could also create :  Perm ds ds'

-- minorToMajor = transpose (lowerPerm reversal)
-- see https://www.tensorflow.org/xla/shapes#minor-to-major_dimension_ordering


--------------------------------------
--




reshape 
  :: forall t ds ds'. Elt t 
  => Reshapable ds ds'
  => Tensor t ds -> Tensor t ds'
reshape = unsafeCoerce

transpose 
  :: forall t ds ds'. Elt t 
  => KnownDims ds
  => Permutable ds ds'
  => Perm (Rank ds) -> Tensor t ds -> Tensor t ds'
transpose p (Tensor v) = Tensor v'
  where
    d = dims @_ @ds
    v' = modifyIdxs d v $ \i m -> 
           remapIdxs p d i $ \d' i' -> 
             M.modify m (const $ v V.! fromIdxs d' (_permuted p i)) (fromIdxs d' i')

{-
vector :: forall n . KnownDim n => KnownNat n => [HsReal] -> ExceptT String IO (Tensor '[n])
vector rs
  | genericLength rs == dimVal (dim :: Dim n) = asStatic <$> Dynamic.vectorEIO rs
  | otherwise = ExceptT . pure $ Left "Vector dimension does not match length of list"

unsafeVector :: (KnownDim n, KnownNat n) => [HsReal] -> IO (Tensor '[n])
unsafeVector = fmap (either error id) . runExceptT . vector
-}

fromVector :: Elt t => Dims ds -> Vector t -> Maybe (Tensor t ds)
fromVector v = undefined

fromVector' :: (Elt t, KnownDims ds) => Dims ds -> Vector t -> Maybe (Tensor t ds)
fromVector' v = undefined

fromScalar :: Elt t => t -> Tensor t '[]
fromScalar = constant (dims @_ @'[])

constant :: Elt t => Dims ds -> t -> Tensor t ds
constant ds t = fill ds $ const t

fill :: forall t ds. Elt t => Dims ds -> (Idxs ds -> t) -> Tensor t ds
fill ds f = Tensor $ V.create $ do
  mv <- M.new (fromIntegral $ totalDim ds)
  let act ix = M.write mv (minorToMajor ds ix) $ f ix
  overDimIdx_ ds act
  return mv

{-

fill :: Elt t => Dims ds -> (Int -> t) -> Vector t
fill dims act = 
  case dims of 
    Reverse dims' -> fill' dims act
   
fill' :: Elt t => Dims ds -> (Int -> t) -> Vector t
fill' dims act = V.create $ do
  v <- M.new (fromIntegral $ totalDim dims)
  let f i = M.write v i $ act i
  overDimOff_ dims f 0 1 -- either control the offset ourselves or fix type issues
  return v

TODO add tests:
> mod ds idxs m = M.swap m 0 (fromIdxs ds idxs)
> v = V.fromList [1..12]
> v
[1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0]
> modifyIdxs (dims @_ @'[2, 2, 3]) v $ mod (dims @_ @'[2, 2, 3])
[12.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0]

> modifyIdxs (dims @_ @'[2, 2, 3]) v (\_ m -> M.set m 0)
[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]

-}


modifyIdxs :: forall t ds. Storable t => Dims ds -> Vector t -> (forall s. Idxs ds -> M.MVector s t -> ST s ()) -> Vector t
modifyIdxs dims v f = V.modify (\mv -> overDimIdx_ dims (\i -> f i mv)) v




{- unit tests
 -
 -

d233 = (dims @_ @'[2,3,3])
d332 = (dims @_ @'[3,3,2])

ttt :: Perm 3
ttt = transposition @2 @3

w = fill d233 $ majorToMinor d233 :: Vector Int -- col major
w' = fill d233 $ minorToMajor d233 :: Vector Int -- row major



--test - transposing a matrix
--
re :: Perm (Rank '[2, 4])
re = reversal

d24 = (dims @_ @'[2, 4])
d42 = (dims @_ @'[4, 2])

t :: Tensor Int '[2, 4]
t = fill d24 $ majorToMinor d24 -- col major

t' :: Tensor Int '[2, 4] 
t' = fill d24 $ minorToMajor d24 -- row major

t'' :: Tensor Int '[4, 2] 
t'' = fill d42 $ minorToMajor d42 -- row major

res = reshape t :: Tensor Int '[4, 2]
res' = transpose re t' :: Tensor Int '[4, 2]

transpose t' == reshape t && transpose t == reshape t'

--test - reversing a cube

re :: Perm (Rank '[3, 3, 3])
re = reversal

d333 = (dims @_ @'[3,3,3])

t :: Tensor Int '[3, 3, 3] 
t = fill d333 $ majorToMinor d333 -- col major

t' :: Tensor Int '[3, 3, 3] 
t' = fill d333 $ minorToMajor d333 -- row major

res :: Tensor Int '[3, 3, 3] 
res = transpose re t'

transpose re t' == reshape t && transpose re t == reshape t'



--test 2:

res = modifyIdx d233 (modify (filterIdx triangular'' ttt) d233) w'
res == [1,4,7,2,5,8,3,6,9,10,13,16,11,14,17,12,15,18]


--test 
a :: Maybe (Idxs '[2, 3, 3])
a = idxsFromWords [2, 1, 1]  

b :: Maybe (Idxs '[2, 3, 3])
b = idxsFromWords [2, 3, 3] 

check :: Dims ds -> Idxs ds -> Idxs ds -> Perm (Rank ds) -> (Perm (Rank ds) -> Idxs ds -> Idxs ds) -> [(Int, Int)]
check d i j p f = foldDimPartIdx i j acc []
  where 
    acc i l = if (i/=f p i) then (1 + minorToMajor d i, 1 + minorToMajor d (f p i)) : l else l

res = liftM2 (\i j -> check d233 i j ttt $ filterIdx triangular'') a b

res == Just [(11,13),(12,16),(15,17)]


-}




{-

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
  => KnownDims x
  => KnownDims y
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
  => KnownDims x
  => KnownDims y
  => T (x ++ [m] ++ y) 
  -> T '[m, n]
  -> T (x ++ n ++ y)

-- #>
productR
  :: All KnownDim '[a, b, c]
  => KnownDims x
  => T (a :+ b :+ x)
  -> T (b :+ c :+ x)
  -> T (a :+ c :+ x)
productR = undefined

-- <#
-- same as tf.matmul
productN
  :: All KnownDim '[a, b, c]
  => KnownDims x
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


