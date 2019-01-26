{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}

module Main where

import Data.Tensor --(T(..), fromVector, shape)
import Data.Bits
import qualified Data.Vector as V (fromList, Vector(..))

import GHC.TypeLits

data Ten (d :: [Nat]) a where
  Ten :: (Show a, Floating a) => a -> Ten d a

instance Show (Ten d a) where
  show (Ten a) = show a

t :: Ten '[3,4] Float
t = Ten 5

u :: V.Vector Int
u = V.fromList [1..10]

main :: IO ()
main = case (constant u :: Maybe (I '[2,5])) of
  Nothing -> print "nope"
  Just t -> do
    print $ shape t
    print $ t + t
    print $ t .&. t
    print $ t `xor` t
    print $ bitSize t
    print $ rotate t 1
    print $ t == t
    let t' :: I '[2,2] 
        t' = fromIntegral 1
    print $ t'


{-
main = VS.withSized u $ \v -> do 
  let t :: T '[2,5] 
      t = constant v
  print $ shape t

-}
