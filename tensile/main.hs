{-# LANGUAGE DataKinds, KindSignatures, OverloadedLists, GADTs #-}

module Main where


import Numeric.Tensile.Tensor --(T(..), fromVector, shape)
import Data.Bits
import Data.Int

import Numeric.Tensile.Dimensions.Types
import Numeric.Tensile.Operations.Linear.Unlifted
import Numeric.Tensile.Operations.Linear.Internal --TODO: remove

u :: [Int32]
u = [1..10]

v :: [Float]
v = replicate 8 1.0

w :: [Float]
w = [1..16]

w' :: [Float]
w' = replicate 16 1.0

println = putStr "\n"

main :: IO ()
main = return ()
{-
main = do
  case (fromList u :: Maybe (I '[2, 5])) of
    Nothing -> print "nope"
    Just t -> do
      print t
      print $ shape t
      print $ t + t
      --print $ t .&. t
      --print $ t `xor` t
      --print $ rotate t 1
      println
   
  case (fromList v :: Maybe (T '[2,4]), fromList v :: Maybe (T '[4,2])) of
    (Just t, Just t') -> do
      print t'
      print $ shape t'
      print $ t `matmul` t'    
      --print $ t * t
      --print $ sin t
      --print $ t `gte` t 
      println
    _ -> print "nope"

  case (fromList v :: Maybe (T ('[2,2] ++ '[2])), fromList v :: Maybe (T '[2,4])) of
    (Just t, Just t') -> do
      let tt = t `matmul` t' 
          tt' = transpose t'
      print $ tt
      print $ shape tt
      print $ tt'
      print $ shape tt'
      println
    _ -> print "nope"

  case (fromList w :: Maybe (T '[2,8]), fromList w' :: Maybe (T '[8,2])) of
    (Just t, Just t') -> do
      let tt = transpose t
          tt' = t `matmul` t' 
          --tt' = transpose t'
      print $ t
      print $ t'
      print $ tt
      print $ tt'
      --print $ t'
      --print $ tt'
-}
