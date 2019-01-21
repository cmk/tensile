--{-# LANGUAGE DataKinds, KindSignatures #-}

module Main where

import Data.Tensor --(T(..), fromVector, shape)
import Data.Bits
import qualified Data.Vector as V (fromList, Vector(..))

u :: V.Vector Int
u = V.fromList [1..10]

main :: IO ()
main = case (constant u :: Maybe (T '[2,5] Int)) of
  Nothing -> print "nope"
  Just t -> do
    print $ shape t
    print $ t + t
    print $ t .&. t
    print $ t `xor` t
    print $ bitSize t
    print $ rotate t 1

    let t' :: T '[2,2] Float
        t' = fromIntegral 1
    print $ t'


{-
main = VS.withSized u $ \v -> do 
  let t :: T '[2,5] Float
      t = constant v
  print $ shape t

-}
