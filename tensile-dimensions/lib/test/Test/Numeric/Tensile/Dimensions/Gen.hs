module Test.Numeric.Tensile.Dimensions.Gen where

import Data.Maybe (fromJust)
import Hedgehog
import Numeric.Tensile.Dimensions

import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

unsafeSomeDim = fromJust . someDim

gen_dim :: MonadGen m => m SomeDim
gen_dim = G.sized $ \n -> unsafeSomeDim <$> (G.word $ R.linear 1 $ fromIntegral n)

gen_dims :: MonadGen m => Range Word -> m SomeDims
gen_dims r = G.list (fmap fromIntegral r) gen_dim
