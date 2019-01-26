{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}

module Main where


import Data.Tensor --(T(..), fromVector, shape)
import Data.Tensor.Types
import Data.Bits
import Eigen.Matrix (Matrix(..), Vec(..))
import qualified Data.Vector.Storable as V (fromList, Vector(..))
import qualified Eigen.Matrix as E

u :: V.Vector Int
u = V.fromList [1..10]

v :: V.Vector Float
v = V.fromList [1..10]

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
      print $ t == t    

  case (constant v :: Maybe (T '[2,5])) of
    Nothing -> print "nope"
    Just t -> do
      print t
      print $ shape t
      print $ t + t
      print $ t * t
      print $ sin t
      print $ t == t
      let m = toMatrix t :: Matrix 5 2 Float
      print $ E.trace m
      print $ E.squaredNorm m

{-
main = VS.withSized u $ \v -> do 
  let t :: T '[2,5] 
      t = constant v
  print $ shape t

-}
