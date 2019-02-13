module Test.Numeric.Tensile.Dimensions.Gen where

import Hedgehog
import qualified Hedgehog.Gen as G

import Numeric.Tensile.Types (Dims(..), KnownDims(..), SomeDims(..), dims, someDimsVal, withSomeDims)

gen_dims :: MonadGen m => Range Word -> m SomeDims
gen_dims r = someDimsVal <$> G.list (fmap fromIntegral r) (G.word r)

gen_dims' :: MonadGen m => Range Word -> m [Word]
gen_dims' r = G.list (fmap fromIntegral r) (G.word r)
