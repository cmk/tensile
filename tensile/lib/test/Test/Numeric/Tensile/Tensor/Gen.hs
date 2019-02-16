module Test.Numeric.Tensile.Tensor.Gen (
  module Test.Numeric.Tensile.Tensor.Gen,
  module Test.Numeric.Tensile.Tensor.Gen.Internal
) where

import Numeric.Tensile.Tensor
import Data.Vector.Storable (Vector(..),Storable(..))
import Numeric.Tensile.Types (Dims(..), KnownDims(..), SomeDims(..), dims, someDimsVal, withSomeDims)
import Test.Numeric.Tensile.Dimensions.Gen
import Test.Numeric.Tensile.Tensor.Gen.Internal (gen_tensor')
import qualified Data.Vector.Storable as V

import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

gen_tensor :: forall d e m. (KnownDims d, Elt e, MonadGen m) => m e -> m (Tensor d e)
gen_tensor = gen_tensor' $ dims @_ @d

{-

gen_tensor_dynamic :: (Elt e, MonadGen m) => Range Word -> Range e -> (Range e -> m e) -> (forall d. Dims d -> Tensor d e -> Bool) -> m Bool
gen_tensor_dynamic rw re g k = gen_dims rw >>= \(SomeDims d) -> gen_tensor' d re g >>= return . k d

gen_tensor_dynamic' :: (Elt e, MonadGen m) => Range Word -> Range e -> (Range e -> m e) -> (forall d. Dims d -> Tensor d e -> Bool) -> m Bool
gen_tensor_dynamic' rw re g k = gen_dims' rw >>= \d -> withSomeDims d f
  where f d = gen_tensor' d re g >>= return . k d

Divisible:

divide :: (Tensor d e -> (b, c)) -> f b -> f c -> f (Tensor d e)

conquer :: f a


contramap :: (a -> b) -> f b -> f a Source#

(>$) :: b -> f b -> f a 

newtype Predicate e = Predicate { unPredicate :: forall d. Tensor d e -> Bool }


prop_splitDims :: [Word] -> Bool
prop_splitDims n xsys
  | SomeDims dxsys <- someDimsVal xsys
  , Dx dn <- someDimVal n -- TODO: why this causes non-exhaustive patterns in GHC 8.2?
  , (xs, ys) <- splitAt (fromIntegral n) xsys
  = case TL.splitAt dn dxsys of
      (dxs, dys) -> and
        [ listDims dxs == xs
        , listDims dys == ys
        -- , dxsys == TL.concat dxs dys
        ]

integral :: (MonadGen m, Integral a) => Range a -> m a
list :: MonadGen m => Range Int -> m a -> m [a]
word :: MonadGen m => Range Word -> m Word

gen_tensor' :: (MonadGen m, Elt e) => Dims d -> (Range e -> m e) -> Range e -> m (Tensor d e)
gen_tensor' d g r = Tensor <$> gen_vector ran g r
  where ran = R.singleton $ fromIntegral (totalDim d)

gen_tensor_dyn :: forall d e m. (MonadGen m, Elt e, KnownDims d) => (Range e -> m e) -> Range e -> m (Tensor d e)
gen_tensor_dyn = gen_tensor' $ dims @_ @d
-}
