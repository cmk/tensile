module Numeric.Tensile.Operations.Linear.Internal where

import Numeric.Tensile.Tensor.Internal
import Numeric.Tensile.Types
import Numeric.Tensile.Index
import Numeric.Tensile.Permutation
import Unsafe.Coerce (unsafeCoerce)

import Data.Vector.Sized (Vector)
import qualified Data.Finite as F
import qualified Data.Vector as V
import qualified Data.Vector.Sized as Sz
import qualified Data.Vector.Storable as St
import qualified Data.Vector.Storable.Mutable as Mu

import GHC.TypeLits (KnownNat(..), natVal)
-- import qualified Numeric.LinearAlgebra.HMatrix as Ma



-- f :: (Dims d' -> Perm n -> Perm n) -> Perm n -> Tensor d e -> Tensor d e'
-- f dim2Idx perm t = Tensor $ reifyDims (permuteDims perm (dims @_ @d)) $ \p ->
--   modifyIdx (reflect p) (modify (permuteIdxs (dim2Idx (reflect p) perm) _)) (reflect p)) t -- basically make user derive the Idxs d' -> Idxs d'

-- dim2Idx :: Rank d ~ n => Dims d -> Perm n -> Perm n
-- takes a perm on dimensions and derives a perm in indices, eg
-- dim2Idx d p = lowerPerm' ...
-- --
-- otherwise consider using the raw index fold and lowerPerm???
-- could also create :  Perm d d'


--------------------------------------
--

fromSizedVector :: Elt e => Vector (Size d) e -> Tensor d e
fromSizedVector = Tensor . St.convert . Sz.fromSized

toSizedVector :: Elt e => Tensor d e -> Vector (Size d) e
toSizedVector = coerce . St.convert . unTensor
  where coerce :: V.Vector e -> Sz.Vector n e
        coerce = unsafeCoerce

transpose' 
  :: Elt e 
  => Permutable d d'
  => Dims d -> Perm (Rank d) -> Tensor d e -> Tensor d' e
transpose' d p (Tensor v) = Tensor v'
  where v' = modifyIdxs d v $ \i m -> 
               remapIdxs p d i $ \d' i' -> 
                 Mu.modify m (const $ v St.! fromIdxs d' (_permuted p i)) (fromIdxs d' i')

pack
  :: Elt e 
  => Vector n (Tensor (x ++ y) e) -> Tensor (x ++ n :+ y) e
pack = undefined

pack0
  :: forall d e n. Elt e
  => KnownDims d
  => KnownDim n
  => Vector n (Tensor d e) -> Tensor (n :+ d) e
pack0 v = Tensor res
  where d = dims @_ @d
        n = dim @_ @n
        size = product $ listDims d
        res = St.create $ do
          mv <- Mu.new $ fromIntegral $ size * dimVal n 
          flip Sz.imapM_ v $ \i t -> 
            let i' = idxToWord . idxFromFinite $ i
                off = fromIntegral $ i' * size
                v' = unTensor t
                act ix = Mu.write mv (off + fromEnum ix) $ v' St.! (fromEnum ix) -- could use a tensor op instead here
            in overDimIdx_ d act
          return mv

unpack0 
  :: forall d e n. Elt e
  => KnownDims d
  => KnownNat n
  => Tensor (n :+ d) e -> Vector n (Tensor d e)
unpack0 t = Sz.generate f
  where d = dims @_ @d
        --n = natVal (Proxy :: n) --dim @_ @n
        size = fromIntegral $ product $ listDims d
        f i = fill d $ \ix -> 
          let i' = fromIntegral $ F.getFinite i
              off = i' * size
              v = unTensor t 
          in v St.! (off + fromEnum ix)


t :: Vector 4 (Tensor '[2,2] Word)
t = Sz.generate $ \f -> 
  let d = dims @_ @'[2,2]
      i' = idxToWord . idxFromFinite $ f
  in fill d (const i') 

t' :: Tensor '[4,2,2] Word
t' = pack0 t

t'' :: Vector 4 (Tensor '[2,2] Word)
t'' = unpack0 t'
{-

generate :: forall n a. KnownNat n => (Finite n -> a) -> Vector n a

t :: Data.Vector.Sized.Vector 4 (Tensor '[2,2] Word)
t = generate $ \f -> 
  let d = dims @_ @'[2,2]
      i' = idxToWord . idxFromFinite $ f
  in fill d (const i') 

t' :: Tensor '[4,2,2] Word
t' = pack0 t
Sz.imapM_ :: Monad m => (Finite n -> a -> m b) -> Vector n a -> m ()

overDim_ :: Monad m
         => Dims ds -- ^ Shape of a space
         -> (Idxs ds -> Int -> m ()) -- ^ Function to call on each dimension
         -> Int -- ^ Initial offset
         -> Int -- ^ Offset step
         -> m ()

fill :: forall d e. Elt e => Dims d -> (Idxs d -> e) -> Tensor d e
fill d f = Tensor $ V.create $ do
  mv <- M.new $ fromIntegral $ product $ listDims d
  let act ix = M.write mv (minorToMajor d ix) $ f ix
  overDimIdx_ d act
  return mv

-- 
modifyIdxs :: forall d e. Storable e => Dims d -> Vector e -> (forall s. Idxs d -> M.MVector s e -> ST s ()) -> Vector e
modifyIdxs d v f = V.modify (\mv -> overDimIdx_ d (\i -> f i mv)) v


stack ::  Dim n -> Vector n (Tensor (x ++ y) e) -> Tensor (x +: n :+ y) e
unstack :: (KnownDims x, Elt e) => Dim n -> Tensor (x +: n :+ y) e -> Vector n (Tensor (x ++ y) e)

gen# 
  :: forall s t d. PrimBytes t 
  => Int# -- ^ number of elements, not checked!
             --   Avoid using this argument if possible.
  -> (s -> (# s, t #))
  -> s -> (# s, ArrayBase t d #)
gen# n f z0 = go (byteSize @e undefined *# n)
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

-- <#
-- same as tf.matmul
productN
  :: forall a b c x. All KnownDim '[a, b, c]
  => KnownDims x
  => T (x +: a +: b) 
  -> T (x +: b +: c)
  -> T (x +: a +: c)
productN = undefined
  where
    d = dims @_ @x
    --s = unsafeCoerce a

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


