{-# LANGUAGE TemplateHaskell #-}
module Test.Numeric.Tensile.Operations.Linear.Property (
  module Test.Numeric.Tensile.Operations.Linear.Property,
  module Test.Numeric.Tensile.Operations.Linear.Property.Internal
) where

import Hedgehog
import Test.Numeric.Tensile.Operations.Linear.Property.Internal

tests :: IO Bool
tests = checkParallel $$(discover)
