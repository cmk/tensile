{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators #-}

module Numeric.Tensile.Operator where

import Data.ByteString (ByteString)
import Data.Complex (Complex)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16)
import Data.ProtoLens (def)
import Data.Text (Text)
import Prelude hiding (abs, sum, concat)
import qualified Prelude (abs)
import Data.Maybe (fromMaybe)
--import Proto.Tensorflow.Core.Framework.NodeDef (NodeDef, attr, input, op, name)
import Proto.Tensorflow.Core.Framework.NodeDef (NodeDef)
import Proto.Tensorflow.Core.Framework.OpDef_Fields
  ( attr
  , inputArg
  , name
  , op
  )

import Control.Lens (Lens',(.~),(&),(^.),at,anon,iso,non)

import Numeric.Backprop

import TensorFlow.Build
import TensorFlow.Ops
import TensorFlow.Tensor
import TensorFlow.Types
import qualified TensorFlow.GenOps.Core as C

type T a = Tensor Build a


lookupAttr :: Attribute a1 => NodeDef -> Text -> a1
lookupAttr nodeDef attrName = nodeDef ^. attr . at attrName . non def . attrLens

-- TODO: Double check all these /= restrictions after bumping backprop version
squareD
    :: (TensorType a, Num a, a /= Int8, a /= Int16,
        a /= Word8, a /= Word16, a /= ByteString, a /= Bool, Reifies s W)

    => BVar s (T a)
    -> BVar s (T a)
squareD = liftOp1 . op1 $ \x ->
  ( x * x, \dzdy -> dzdy * 2 * x)

{-






mulD 
    :: (TensorType a, Num a, a /= Int8, a /= Int16,
        a /= Word8, a /= Word16, a /= ByteString, a /= Bool, Reifies s W)
    => BVar s (T a)
    -> BVar s (T a)
    -> BVar s (T a)
mulD = liftOp2 . op2 $ \x1 x2 ->
  (x1 * x2, \dzdy -> (dzdy * x2, x1 * dzdy))

addD
    :: (TensorType a, Num a, a /= Bool, a /= Int8, a /= Int16, a /= Word8,
        a /= Word16, a /= ByteString, Reifies s W)
    => BVar s (T a)
    -> BVar s (T a)
    -> BVar s (T a)
addD = liftOp2 . op2 $ \x1 x2 ->
  (x1 + x2, \dzdy -> (dzdy, dzdy))

subD
    :: (TensorType a, Num a, a /= Bool, a /= Int8, a /= Int16, a /= Word8,
        a /= Word16, a /= ByteString, Reifies s W)
    => BVar s (T a)
    -> BVar s (T a)
    -> BVar s (T a)
subD = liftOp2 . op2 $ \x1 x2 ->
  (x1 - x2, \dzdy -> (dzdy, negate dzdy))

matMulD :: (TensorType a, Num a, a /= Int8, a /= Int16, a /= Int64, a /= Word8,
            a /= Word16, a /= ByteString, a /= Bool, Reifies s W )
    => BVar s (T a)
    -> BVar s (T a)
    -> BVar s (T a)
matMulD = 
  let transAttrs a b = (opAttr "transpose_a" .~ a) . (opAttr "transpose_b" .~ b)

  in liftOp2 . op2 $ \x y ->
  (C.matMul x y, \dz -> (C.matMul' (transAttrs False True) dz y, C.matMul' (transAttrs True False) x dz))

myMeanD
  :: (TensorType t, Num t, t /= Bool, t /= Int8, t /= Int16, t /= Word8, t /= Word16, t /= ByteString, Reifies s W) 
    => BVar s (T t)
    -> BVar s (T t)
myMeanD = liftOp1 . op1 $ \x -> (myMean x, id) -- extremely suspect

myMean x = C.mean' id x allAxes
  where allAxes = C.range 0 (C.rank x :: Tensor Build Int32) 1

-}

