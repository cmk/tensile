module Test.Data.Tensor.Generators where

import Data.Tensor.Types (T(..),I(..),B(..))
import Numeric.Tensile.Types (KnownDims(..))

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

rf :: (Fractional a, Ord a) => Range a
rf = R.linearFracFrom 0 (-100) 100

tensor :: KnownDims d => Gen (T d)
tensor = undefined
