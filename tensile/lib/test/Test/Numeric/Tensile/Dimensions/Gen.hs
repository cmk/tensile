module Test.Numeric.Tensile.Dimensions.Gen where

import Hedgehog
import qualified Hedgehog.Gen as G

import Numeric.Tensile.Types (Dims(..), KnownDims(..), SomeDims(..), dims, someDimsVal, withSomeDims)

gen_dims :: MonadGen m => Range Word -> m SomeDims
gen_dims r = someDimsVal <$> gen_dims' r

gen_dims' :: MonadGen m => Range Word -> m [Word]
gen_dims' r = G.list (fmap fromIntegral r) (G.word r)

{-
 - gen_tensor :: forall d e m. (KnownDims d, Elt e, MonadGen m) => Range e -> (Range e -> m e) -> m (Tensor d e)
gen_tensor = gen_tensor' $ dims @_ @d

gen_dynamic :: (Elt e, MonadGen m) => Range Word -> Range e -> (Range e -> m e) -> (forall d. Dims d -> Tensor d e -> Bool) -> m Bool
gen_dynamic rw re g k = gen_dims rw >>= \(SomeDims d) -> gen_tensor' d re g >>= return . k d
-}
