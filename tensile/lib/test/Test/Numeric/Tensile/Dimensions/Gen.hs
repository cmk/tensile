module Test.Numeric.Tensile.Dimensions.Gen where

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

import Numeric.Tensile.Dimensions.Types

gen_dim :: MonadGen m => m SomeDim
gen_dim = G.sized $ \n -> someDimVal <$> (G.word $ R.linear 1 $ fromIntegral n)

gen_dims :: MonadGen m => Range Word -> m [SomeDim]
gen_dims r = G.list (fmap fromIntegral r) gen_dim
