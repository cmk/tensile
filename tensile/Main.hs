--{-# LANGUAGE DataKinds, KindSignatures #-}

module Main where

import Numeric.Tensile.Tensor --(T(..), fromVector, shape)
import qualified Data.Vector as V (fromList, Vector(..))

u :: V.Vector Float
u = V.fromList [1..10]

main :: IO ()
main = case (constant u :: Maybe (T '[2,5] Float)) of
  Nothing -> print "nope"
  Just t -> print $ shape t


{-
main = VS.withSized u $ \v -> do 
  let t :: T '[2,5] Float
      t = constant v
  print $ shape t

-}
