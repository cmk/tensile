{-# LANGUAGE TemplateHaskell #-}
module Test.Numeric.Tensile.Dimensions.Dims.Property where

import Numeric.Tensile.Dimensions
import Test.Numeric.Tensile.Dimensions.Gen
import Test.Numeric.Tensile.Dimensions.Dims.Predicate

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

rw :: Range Word
rw = R.constant 0 100

prop_reify_reflect :: Property
prop_reify_reflect =
  property $ assert . (\d -> withSomeDims d pred_reify_reflect) =<< forAll (gen_dims rw)

tests :: IO Bool
tests = checkParallel $$(discover)
