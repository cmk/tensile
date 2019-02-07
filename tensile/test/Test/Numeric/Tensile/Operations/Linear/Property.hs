{-# LANGUAGE TemplateHaskell #-}
module Test.Numeric.Tensile.Operations.Linear.Property where

import Numeric.Tensile.Tensor
import Numeric.Tensile.Types
import Test.Numeric.Tensile.Operations.Linear.Predicate
import Test.Numeric.Tensile.Tensor.Gen

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

rf :: Range Float
rf = R.linearFracFrom 0 (-100) 100

prop_transposition :: Property
prop_transposition =
  property $ (assert . pred_transposition) =<< forAll (tensor (dims @_ @'[3,3,3]) G.float rf)

tests :: IO Bool
tests = checkParallel $$(discover)
