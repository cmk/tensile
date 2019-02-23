{-# LANGUAGE TemplateHaskell #-}
module Test.Numeric.Tensile.Operations.Linear.Property where

import Numeric.Tensile.Dimensions
import Test.Numeric.Tensile.Operations.Linear.Predicate
import Test.Numeric.Tensile.Tensor.Gen

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

gf :: Gen Float
gf = G.float $ R.linearFracFrom 0 (-100) 100

prop_transpose_involution1 :: Property
prop_transpose_involution1 = property $ do
  t <- forAll $ gen_tensor (dims @'[3,3,3]) gf
  assert $ pred_transpose_involution1 t

prop_transpose_involution2 :: Property
prop_transpose_involution2 = property $ do
  t <- forAll $ gen_tensor (dims @'[5,4,3,2]) gf
  assert $ pred_transpose_involution2 t

prop_product_transpose :: Property
prop_product_transpose = property $ do
  t <- forAll $ gen_tensor (dims @'[2,4]) gf
  u <- forAll $ gen_tensor (dims @'[4,2]) gf
  assert $ pred_product_transpose t u

tests :: IO Bool
tests = checkParallel $$(discover)
