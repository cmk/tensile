--{-# LANGUAGE DataKinds, KindSignatures #-}

module Main where

import Data.Tensor --(T(..), fromVector, shape)
import qualified Data.Vector as V (fromList, Vector(..))

u :: V.Vector Float
u = V.fromList [1..10]

main :: IO ()
main = case (constant u :: Maybe (T '[2,5] Float)) of
  Nothing -> print "nope"
  Just t -> do
    print $ shape t
    print $ t + t
    let t' :: T '[2,2] Float
        t' = fromIntegral 1
    print $ t'


{-
main = VS.withSized u $ \v -> do 
  let t :: T '[2,5] Float
      t = constant v
  print $ shape t

-}
