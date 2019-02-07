module Test.Numeric.Tensile.Operations.Linear.Predicate where

import Numeric.Tensile.Tensor
import Numeric.Tensile.Types
import Numeric.Tensile.Permutation (Perm(..), reversal, reversal')
import Numeric.Tensile.Operations.Linear.Unlifted (transpose, transpose')
import Test.Numeric.Tensile.Tensor.Gen

import Numeric.Type.List (Reverse(..))
import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R
import qualified Data.Vector.Storable as V




pred_transposition :: forall e. (Elt e, Eq e) => Tensor '[3,3,3] e -> Bool
pred_transposition t = t == (f . f) t
  where f :: Tensor '[3,3,3] e -> Tensor '[3,3,3] e
        f = transpose (reversal @'[3,3,3]) 
        



{- unit tests
 -
 -

TODO

rev :: forall (d :: [Nat]). Dims d -> Dims (Reverse d)
rev = unsafeCoerce . reverse . unsafeCoerce

pred_transposition' :: forall d e. (Elt e, Eq e, V.Storable e) => Dims d -> Tensor d e -> Bool
pred_transposition' d t = undefined -- toVector t == toVector t'
  where --r :: Perm 3
        --r = reversal'
        f :: Tensor d e -> Tensor (Reverse d) e
        f = unsafeCoerce $ transpose' d (reversal' d)

        g :: forall d. Tensor (Reverse d) e -> Tensor d e
        g = transpose' (rev d) (reversal' (rev d))

        t' = g . f $ t

-- minorToMajor = transpose (lowerPerm reversal)
-- see https://www.tensorflow.org/xla/shapes#minor-to-major_dimension_ordering



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

tensor :: (MonadGen m, Elt e) => Dims d -> (Range e -> m e) -> Range e -> m (Tensor d e)
tensor d g r = Tensor <$> genVectorOf ran g r
  where ran = R.singleton $ fromIntegral (totalDim d)

listDims :: Dims xs -> [Word]
listDims = unsafeCoerce#

d233 = (dims @_ @'[2,3,3])
d332 = (dims @_ @'[3,3,2])

ttt :: Perm 3
ttt = transposition @2 @3

w = fill d233 $ majorToMinor d233 :: Vector Int -- col major
w' = fill d233 $ minorToMajor d233 :: Vector Int -- row major



--test - transposing a matrix
--
re :: Perm (Rank '[2, 4])
re = reversal

d24 = (dims @_ @'[2, 4])
d42 = (dims @_ @'[4, 2])

t :: Tensor Int '[2, 4]
t = fill d24 $ majorToMinor d24 -- col major

t' :: Tensor Int '[2, 4] 
t' = fill d24 $ minorToMajor d24 -- row major

t'' :: Tensor Int '[4, 2] 
t'' = fill d42 $ minorToMajor d42 -- row major

res = reshape t :: Tensor Int '[4, 2]
res' = transpose re t' :: Tensor Int '[4, 2]

transpose t' == reshape t && transpose t == reshape t'

--test - reversing a cube

re :: Perm (Rank '[3, 3, 3])
re = reversal

d333 = (dims @_ @'[3,3,3])

t :: Tensor Int '[3, 3, 3] 
t = fill d333 $ majorToMinor d333 -- col major

t' :: Tensor Int '[3, 3, 3] 
t' = fill d333 $ minorToMajor d333 -- row major

res :: Tensor Int '[3, 3, 3] 
res = transpose re t'

transpose re t' == reshape t && transpose re t == reshape t'

-}
 
