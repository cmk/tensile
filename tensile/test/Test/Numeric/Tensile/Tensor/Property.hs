{-# LANGUAGE TemplateHaskell #-}
module Test.Numeric.Tensile.Tensor.Property where

import Numeric.Tensile.Dimensions
import Test.Numeric.Tensile.Tensor.Gen

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

gf :: Gen Float
gf = G.float $ R.linearFracFrom 0 (-100) 100

{-
prop_cubic_transpose :: Property
prop_cubic_transpose =
  property $ assert . pred_cubic_transpose =<< forAll (gen_tensor (dims @'[3,3,3]) gf)

prop_prism_transpose :: Property
prop_prism_transpose =
  property $ assert . pred_prism_transpose =<< forAll (gen_tensor (dims @'[5,4,3,2]) gf)
-}

tests :: IO Bool
tests = checkParallel $$(discover)     
