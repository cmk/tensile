{-# LANGUAGE TemplateHaskell #-}
module Test.Numeric.Tensile.Operations.Linear.Property where

import Numeric.Tensile.Types
import Test.Numeric.Tensile.Operations.Linear.Predicate
import Test.Numeric.Tensile.Tensor.Gen

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

rf :: Range Float
rf = R.linearFracFrom 0 (-100) 100

prop_cubic_transpose :: Property
prop_cubic_transpose =
  property $ assert . pred_cubic_transpose =<< forAll (gen_tensor' (dims @_ @'[3,3,3]) rf G.float)

prop_prism_transpose :: Property
prop_prism_transpose =
  property $ assert . pred_prism_transpose =<< forAll (gen_tensor' (dims @_ @'[5,4,3,2]) rf G.float)

tests :: IO Bool
tests = checkParallel $$(discover)
