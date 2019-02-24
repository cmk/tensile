module Numeric.Tensile.Operations.Linear.Internal where

import Data.Vector.Sized (Vector)
import Numeric.Tensile.Dimensions
import Numeric.Tensile.Tensor.Internal
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Finite as F
import qualified Data.Vector.Sized as N
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as M
import qualified Math.Combinat.Permutations as P
import qualified TensorFlow.GenOps.Core as O
import qualified TensorFlow.Ops as O (constant, vector)



transpose 
  :: Elt e 
  => Permutable d d'
  => Dims d -> Perm (Rank d) -> Tensor d e -> Tensor d' e
transpose d (Perm p) (Tensor t) = Tensor $ (flip O.transpose) (O.vector w) t
  where v = [0.. fromIntegral $ rank d] :: [IVal]
        w = P.permuteList p v

{-
transpose 
  :: Elt e 
  => Permutable d d'
  => Dims d -> Perm (Rank d) -> Tensor d e -> Tensor d' e
transpose d (Perm p) (Tensor t) = Tensor $ (flip O.transpose) (f w) t
  where v = [0.. fromIntegral $ rank d] :: [IVal]
        w = P.permuteList p v
        f = O.constant (toShape d)
-}

(<#>) 
  :: forall a b c x y z. ()
  => KnownDim a
  => KnownDim b
  => KnownDim c
  => KnownDims x
  => KnownDims y
  => x ~ '[a,b]
  => y ~ '[b,c]
  => z ~ '[a,c]  
  => T x -> T y -> T z
(<#>) x y = reflectDims2 @x @y (\d1 d2 -> product' d1 d2 x y)
--Tensor $ O.matMul a b



product' :: Dims x -> Dims y -> T x -> T y -> T z
product' x y (Tensor a) (Tensor b) = Tensor $ O.matMul a b


{-




(<#>) 
  :: forall m x y. KnownDim m
  => KnownDims x
  => KnownDims y
  => T (x +: m) -> T (m :+ y) -> T (x ++ y)

(<#) 
  :: All KnownDim '[a, b, c]
  => KnownDims x
  => T (x +: a +: b) -> T (x +: b +: c) -> T (x +: a +: c)
(<#) (Tensor a) (Tensor b) = Tensor $ O.matMul a b

(#>)
  :: All KnownDim '[a, b, c]
  => KnownDims x
  => T (a :+ b :+ x) -> T (b :+ c :+ x) -> T (a :+ c :+ x)
(#>) (Tensor a) (Tensor b) = Tensor $ O.matMul a b

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
    d = dims @x
    --s = unsafeCoerce a

product 
  :: forall m x y. KnownDim m
  => KnownDims x
  => KnownDims y
  => T (x +: m) -> T (m :+ y) -> T (x ++ y)
product t u
    | I# m <- fromIntegral $ dimVal @m
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


-}


