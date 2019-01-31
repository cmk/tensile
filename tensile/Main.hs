{-# LANGUAGE DataKinds, KindSignatures, OverloadedLists, GADTs #-}

module Main where


import Data.Tensor --(T(..), fromVector, shape)
import Data.Tensor.Types
import Data.Bits

import Numeric.Dimensions
import Numeric.Tensile.Operations.Linear.Unlifted
import Numeric.Tensile.Operations.Linear.Internal --TODO: remove

u :: [Word]
u = [1..10]

v :: [Float]
v = replicate 8 1.0

w :: [Float]
w = [1..16]

w' :: [Float]
w' = replicate 16 1.0

println = putStr "\n"

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
      println
   

  case (constant v :: Maybe (T '[2,4]), constant v :: Maybe (T '[4,2])) of
    (Just t, Just t') -> do
      print t'
      print $ shape t'
      print $ t `matmul` t'    
      --print $ t * t
      --print $ sin t
      --print $ t `gte` t 
      println
    _ -> print "nope"

  case (constant v :: Maybe (T ('[2,2] ++ '[2])), constant v :: Maybe (T '[2,4])) of
    (Just t, Just t') -> do
      let tt = t <#> t' 
          tt' = transpose t'
      print $ tt
      print $ shape tt
      print $ tt'
      print $ shape tt'
      println
    _ -> print "nope"

  case (constant w :: Maybe (T '[2,8]), constant w' :: Maybe (T '[8,2])) of
    (Just t, Just t') -> do
      let tt = transpose t
          tt' = t <#> t' 
          --tt' = transpose t'
      print $ t
      print $ t'
      print $ tt
      print $ tt'
      --print $ t'
      --print $ tt'

