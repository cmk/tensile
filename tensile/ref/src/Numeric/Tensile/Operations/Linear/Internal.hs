module Numeric.Tensile.Operations.Linear.Internal where

import Numeric.Tensile.Tensor.Internal
import Data.Vector.Storable (Vector(..), Storable(..))
import Numeric.Tensile.Types
import Numeric.Tensile.Index
import Numeric.Tensile.Permutation
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M






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






transpose' 
  :: forall d d' e. Elt e 
  => Permutable d d'
  => Dims d -> Perm (Rank d) -> Tensor d e -> Tensor d' e
transpose' d p (Tensor v) = Tensor v'
  where
    v' = modifyIdxs d v $ \i m -> 
           remapIdxs p d i $ \d' i' -> 
             M.modify m (const $ v V.! fromIdxs d' (_permuted p i)) (fromIdxs d' i')




{-



--test 2:

res = modifyIdx d233 (modify (filterIdx triangular'' ttt) d233) w'
res == [1,4,7,2,5,8,3,6,9,10,13,16,11,14,17,12,15,18]


--test 
a :: Maybe (Idxs '[2, 3, 3])
a = idxsFromWord [2, 1, 1]  

b :: Maybe (Idxs '[2, 3, 3])
b = idxsFromWord [2, 3, 3] 

check :: Dims d -> Idxs d -> Idxs d -> Perm (Rank d) -> (Perm (Rank d) -> Idxs d -> Idxs d) -> [(Int, Int)]
check d i j p f = foldDimPartIdx i j acc []
  where 
    acc i l = if (i/=f p i) then (1 + minorToMajor d i, 1 + minorToMajor d (f p i)) : l else l

res = liftM2 (\i j -> check d233 i j ttt $ filterIdx triangular'') a b

res == Just [(11,13),(12,16),(15,17)]


-}




{-

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


