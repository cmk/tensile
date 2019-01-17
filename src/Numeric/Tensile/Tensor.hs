{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies,  AllowAmbiguousTypes, GADTs, UndecidableInstances #-}
module Numeric.Tensile.Tensor where

import Data.Kind (Type)
import GHC.TypeLits (Nat, type (*), type (<=), KnownNat)

import Data.Word (Word)
import Numeric.Dimensions
import Numeric.Type.List (All)


import qualified TensorFlow.Ops as O
import TensorFlow.Tensor
import TensorFlow.Types
import TensorFlow.Build

import qualified TensorFlow.GenOps.Core as C

import Data.Maybe (isJust)

type TF = T (Tensor Build)



--reshape 
sameShape :: forall f s s' e e'. Dimensions s => Dimensions s' => T f s e -> T f s' e' -> Bool
sameShape _ _ = isJust ev
  where ev = sameDims (dims @Nat @s) (dims @Nat @s')

--need type-level proof that sizes are equal, not shapes
--withEvidence :: Evidence a -> (a => r) -> r 
reshape :: forall f s s' e. Dimensions s => Dimensions s' => Dims s -> TF s' e -> Maybe (TF s e)
reshape d t = ev >>= \e -> withEvidence e f
  where ev = sameDims d (dims @Nat @s) 
        f = undefined -- make idx tensor with ev / TF.constant and feed to TF.reshape
{-
constant :: TensorType a => Shape -> [a] -> Tensor Build a
reshape :: (TensorType t, OneOf ((:) * Int32 ((:) * Int64 ([] *))) tshape)	 
=> Tensor v'1 t	
-> Tensor v'2 tshape	
-> Tensor Build t

-}

-- see https://ghc.haskell.org/trac/ghc/ticket/4009
newtype T :: (Type -> Type) -> [Nat] -> Type -> Type where T :: f e -> T f s e deriving Show 

-- | return the a runtime shape representing the dimensions of a tensor.
shape :: forall f s e. Dimensions s => T f s e -> [Int]
shape _ = fromIntegral <$> listDims (dims @Nat @s)
  --case KnownDims of (d :: Dims '[5]) -> listDims d

-- | Product of all dimension sizes @O(Length xs)@.
size :: forall f s e. Dimensions s => T f s e -> Int
size = product . shape





--
{-
 -

eye :: forall d1 d2 . (All KnownDim '[d1, d2]) => Tensor '[d1, d2]
eye = asStatic $ Dynamic.eye (dim :: Dim d1) (dim :: Dim d2)

constant :: forall d . Dimensions d => HsReal -> Tensor d
constant = asStatic . Dynamic.constant (dims :: Dims d)

https://backprop.jle.im/07-performance.html
softMax
    :: (KnownNat n, Reifies s W)
    => BVar s (R n)
    -> BVar s (R n)
softMax x = konst (1 / totx) * expx
  where
    expx = exp x
    totx = sumElements expx

crossEntropy
    :: (KnownNat n, Reifies s W)
    => R n
    -> BVar s (R n)
    -> BVar s Double
crossEntropy x y = -(log y `dot` auto x)

softMaxCrossEntropy'
    :: (KnownNat n, Reifies s W)
    => R n
    -> BVar s (R n)
    -> BVar s Double
softMaxCrossEntropy' x = liftOp1 . op1 $ \y ->
    let expy     = exp y
        toty     = HU.sumElements (H.extract expy)
        softMaxY = H.konst (1 / toty) * expy
        smce     = -(log softMaxY `H.dot` x)
    in  ( smce
        , \d -> H.konst d * (softMaxY - x)
        )

-}
