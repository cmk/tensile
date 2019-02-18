{-# LANGUAGE TemplateHaskell #-}
module Test.Numeric.Tensile.Dimensions.Idxs.Property where

import Numeric.Tensile.Dimensions
import Test.Numeric.Tensile.Dimensions.Gen
import Test.Numeric.Tensile.Dimensions.Idxs.Predicate

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

rw :: Range Word
rw = R.constant 0 100

prop_max_diffIdx :: Property
prop_max_diffIdx =
  property $ assert . (\d -> withSomeDims d pred_max_diffIdx) =<< forAll (gen_dims rw)

tests :: IO Bool
tests = checkParallel $$(discover)
