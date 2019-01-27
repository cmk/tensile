{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}

module Main where


import Data.Tensor --(T(..), fromVector, shape)
import Data.Tensor.Types
import Data.Bits

u :: [Int]
u = [1..10]

v :: [Float]
v = [1..10]

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
   

  case (constant v :: Maybe (T '[2,5])) of
    Nothing -> print "nope"
    Just t -> do
      print t
      print $ shape t
      print $ t + t
      print $ t * t
      print $ sin t
      print $ t `gte` t 
