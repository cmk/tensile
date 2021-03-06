{-# LANGUAGE TemplateHaskell #-}
module Test.Numeric.Tensile.Dimensions.Idxs.Property where

import Numeric.Tensile.Dimensions
import Test.Numeric.Tensile.Dimensions.Gen
import Test.Numeric.Tensile.Dimensions.Idxs.Predicate

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

dims_large :: Range Word
dims_large = R.constant 0 10

dims_small :: Range Word
dims_small = R.constant 0 5

prop_size_idxs :: Property
prop_size_idxs = property $ do
    d <- forAll $ gen_dims_small dims_small
    assert $ withSomeDims d pred_size_idxs

prop_max_diff_idxs :: Property
prop_max_diff_idxs = property $ 
  assert . (\d -> withSomeDims d pred_max_diff_idxs) =<< forAll (gen_dims dims_large)

prop_max_diff_idxs2 :: Property
prop_max_diff_idxs2 = property $ 
  assert . (\d -> withSomeDims d pred_max_diff_idxs2) =<< forAll (gen_dims dims_large)

prop_modulus_idxs :: Property
prop_modulus_idxs = property $ 
  assert . (\d -> withSomeDims d pred_modulus_idxs) =<< forAll (gen_dims_small dims_small)

prop_transpose_idxs :: Property
prop_transpose_idxs = property $ 
  assert . (\d -> withSomeDims d pred_transpose_idxs) =<< forAll (gen_dims_small dims_small)

tests :: IO Bool
tests = checkParallel $$(discover)
