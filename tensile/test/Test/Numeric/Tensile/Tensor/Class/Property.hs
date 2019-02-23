{-# LANGUAGE TemplateHaskell #-}
module Test.Numeric.Tensile.Tensor.Property where

import Numeric.Tensile.Dimensions
import Test.Numeric.Tensile.Tensor.Gen

import Hedgehog
-- import Hedgehog.Classes
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

gf :: Gen Float
gf = G.float $ R.linearFracFrom 0 (-100) 100

{- TODO need ghc >= 8.5 or preferably a PR to hedgehog-classes. try on vector-based build first
tests_eq  :: IO Bool
tests_eq = lawsCheck $ eqLaws (gen_tensor (dims @'[3,3,3]) gf)

tests :: IO Bool
tests = tests_eq 
-}



