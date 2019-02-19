module Test.Numeric.Tensile.Dimensions.Gen where

import Data.Maybe (fromJust)
import Hedgehog
import Numeric.Tensile.Dimensions

import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

-- We guarantee that the arg > 0.
unsafeSomeDim :: Word -> SomeDim
unsafeSomeDim = fromJust . someDim

gen_dim :: MonadGen m => m SomeDim
gen_dim = G.sized $ \n -> unsafeSomeDim <$> (G.word $ R.linear 1 $ fromIntegral n)

gen_dim_small :: MonadGen m => m SomeDim
gen_dim_small = unsafeSomeDim <$> (G.word $ R.linear 1 5)

gen_dims :: MonadGen m => Range Word -> m SomeDims
gen_dims r = G.list (fmap fromIntegral r) gen_dim

gen_dims_small :: MonadGen m => Range Word -> m SomeDims
gen_dims_small r = G.list (fmap fromIntegral r) gen_dim_small
