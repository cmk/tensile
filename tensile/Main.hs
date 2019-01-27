{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}

module Main where


import Data.Tensor --(T(..), fromVector, shape)
import Data.Tensor.Types
import Data.Bits

import Numeric.Tensile.Linear.Operations

u :: [Int]
u = [1..10]

v :: [Float]
v = replicate 8 1.0

main :: IO ()
main = do
  case (constant u :: Maybe (I '[2,5])) of
    Nothing -> print "nope"
    Just t -> do
      print t
      print $ shape t
      print $ t + t
      print $ t .&. t
      print $ t `xor` t
      print $ bitSize t
      print $ rotate t 1
   

  case (constant v :: Maybe (T '[2,4]), constant v :: Maybe (T '[4,2])) of
    (Just t, Just t') -> do
      print t'
      print $ shape t'
      print $ t `matmul` t'    
      --print $ t * t
      --print $ sin t
      --print $ t `gte` t 
    _ -> print "nope"

  case (constant v :: Maybe (T '[2,2,2]), constant v :: Maybe (T '[2,4])) of
    (Just t, Just t') -> do
      let tt = t `matmul` t' 
      print $ tt
      print $ shape tt
    _ -> print "nope"

