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
{-# LANGUAGE UndecidableInstances   #-}

module Numeric.Tensile.Operations.Linear.Internal where

import Data.Tensor.Types
import Data.Tensor.Internal.Array

import GHC.Base
import Numeric.Dimensions
import Numeric.DataFrame.Internal.Array.Family.ArrayBase
import Numeric.PrimBytes

{-

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
-- #>
matmulR
  :: All KnownDim '[a, b, c]
  => T (a :+ b :+ x)
  -> T (b :+ c :+ x)
  -> T (a :+ c :+ x)
matmulR = undefined

-- <#
matmulL
  :: All KnownDim '[a, b, c]
  => T (x +: a +: b) 
  -> T (x +: b +: c)
  -> T (x +: a +: c)
matmulL = undefined


infixl 7 <#>
(<#>)  
  :: forall a b ab x. KnownDim x
  => ConcatList a b ab -- witness 'a ++ b ~ ab'
  => Dimensions a
  => Dimensions b
  => T (a +: x) -> T (x :+ b) -> T ab
(<#>) = matmul

-- | Tensor contraction.
--   In particular:
--     1. matrix-matrix product
--     2. matrix-vector or vector-matrix product
--     3. dot product of two vectors.

matmul 
  :: forall a b ab x. KnownDim x
  => Dimensions a
  => Dimensions b
  => T (a +: x) -> T (x :+ b) -> T (a ++ b)
matmul x y
    | I# m <- fromIntegral $ dimVal' @x
    , I# n <- fromIntegral $ totalDim' @a
    , I# k <- fromIntegral $ totalDim' @b
    , nk <- n *# k
    = let loop1 i j l r | isTrue# (l ==# m) = r
                        | otherwise = loop1 i j (l +# 1#)
                          (r + ix# (i +# n *# l) x * ix# (l +# m *# j) y)

          loop2 (T# i j) | isTrue# (j ==# k) = (# T# i j, 0 #)
                         | isTrue# (i ==# n) = loop2 (T# 0# (j +# 1#))
                         | otherwise = (# T# (i +# 1#) j, loop1 i j 0# 0 #)
      in case gen# nk loop2 (T# 0# 0#) of
          (# _, r #) -> r

data T# = T# Int# Int#

