module Test.Numeric.Tensile.Operations.Linear.Predicate where

import Numeric.Tensile.Tensor
import Numeric.Tensile.Dimensions
--import Numeric.Tensile.Dimensions.Types
--import Numeric.Tensile.Dimensions.Perm (Perm(..), reversal)
import Numeric.Tensile.Operations.Linear.Internal 
import Test.Numeric.Tensile.Tensor.Gen

import qualified Data.Vector.Storable as V
import qualified Numeric.Tensile.Operations.Linear.Unlifted as U

-- pred_transpose_involution1
pred_transpose_involution1 :: forall e. (Elt e, Eq e) => Tensor '[3,3,3] e -> Bool
pred_transpose_involution1 t = t == (f . f) t
  where 
    f :: Tensor '[3,3,3] e -> Tensor '[3,3,3] e
    f = U.transpose $ reflectDims @'[3,3,3] reversal
        
pred_transpose_involution2 :: forall e. (Elt e, Eq e) => Tensor '[5,4,3,2] e -> Bool
pred_transpose_involution2 t = t == (g . f) t
  where 
    f :: Tensor '[5,4,3,2] e -> Tensor '[2,3,4,5] e
    f = U.transpose $ reflectDims @'[5,4,3,2] reversal

    g :: Tensor '[2,3,4,5] e -> Tensor '[5,4,3,2] e
    g = U.transpose $ reflectDims @'[2,3,4,5] reversal



-- (t * u)^T == u^T * t^T,
pred_product_transpose :: T '[2,4] -> T '[4,2] -> Bool
pred_product_transpose t u = f (t <#> u) == g u <#> h t
  where
    f :: T '[2,2] -> T '[2,2]
    f = U.transpose $ reflectDims @'[2,2] reversal

    g :: T '[4,2] -> T '[2,4]
    g = U.transpose $ reflectDims @'[4,2] reversal

    h :: T '[2,4] -> T '[4,2]
    h = U.transpose $ reflectDims @'[2,4] reversal


pred_product_transpose' :: T '[2,4] -> T '[4,2] -> Bool
pred_product_transpose' t u = f (t <#> u) == g u <#> h t
  where
    f :: T '[2,2] -> T '[2,2]
    f = transpose (dims @'[2,2]) $ reversal (dims @'[2,2])

    g :: T '[4,2] -> T '[2,4]
    g = transpose (dims @'[4,2]) $ reversal (dims @'[4,2]) 

    h :: T '[2,4] -> T '[4,2]
    h = transpose (dims @'[2,4]) $ reversal (dims @'[2,4]) 
{- unit tests
 -
 -

 p = liftA2 (<#>) t u
p' = fmap f p
 u' = fmap g u
 t' = fmap h t
q = liftA2 (<#>) u' t'

TODO

reifyDims :: forall r. [Word] -> (forall (d :: [Nat]). KnownDims d => Proxy d -> r) -> r
reifyDims d k = unsafeCoerce (MagicDims k :: MagicDims r) d Proxy


pred_transposition' :: (Elt e, Eq e, V.Storable e) => Dims d -> Tensor d e -> Bool
pred_transposition' d t = b
  where
    w = fromDims d
    s = product w
    b = withSomeDims (reverse w) $ \d' -> 
          withSomeDims w $ \d'' ->
                let t'' = transpose d'' mempty . transpose d' mempty $ t
                in  t'' == unsafeCoerce t

pred_transposition' :: forall d e. (Elt e, Eq e) => Dims d -> Tensor d e -> Bool
pred_transposition' d t = t'' == t
  where
    w = fromDims d
    t' = reifyDims (reverse w) $ \d -> transpose (reflect d) (reversal' $ reflect d) t
    t'' = reifyDims w $ \d ->transpose (reflect d) (reversal' $ reflect d) $ t'


pred_transposition' :: (Elt e, Eq e, V.Storable e) => Dims d -> Tensor d e -> Bool
pred_transposition' d t = b
  where
    w = fromDims d
    s = product w
    b = withSomeDims (reverse w) $ \d' -> 
          withSomeDims w $ \d'' ->
                let t'' = toVector . transpose d'' mempty . transpose d' mempty $ t
                in  t'' == toVector t


pred_transposition' :: forall d e. (Elt e, Eq e, V.Storable e) => Dims d -> Tensor d e -> Bool
pred_transposition' d t = undefined -- toVector t == toVector t'
  where --r :: Perm 3
        --r = reversal'
        f :: Tensor d e -> Tensor (Reverse d) e
        f = unsafeCoerce $ transpose d (reversal' d)

        g :: forall d. Tensor (Reverse d) e -> Tensor d e
        g = transpose (rev d) (reversal' (rev d))

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
        [ fromDims dxs == xs
        , fromDims dys == ys
        -- , dxsys == TL.concat dxs dys
        ]

tensor :: (MonadGen m, Elt e) => Dims d -> (Range e -> m e) -> Range e -> m (Tensor d e)
tensor d g r = Tensor <$> genVectorOf ran g r
  where ran = R.singleton $ fromIntegral (totalDim d)

fromDims :: Dims xs -> [Word]
fromDims = unsafeCoerce#

d233 = (dims @'[2,3,3])
d332 = (dims @'[3,3,2])

ttt :: Perm 3
ttt = transposition @2 @3

w = fill d233 $ majorToMinor d233 :: Vector Int -- col major
w' = fill d233 $ minorToMajor d233 :: Vector Int -- row major



--test - transposing a matrix
--
re :: Perm (Rank '[2, 4])
re = reversal

d24 = (dims @'[2, 4])
d42 = (dims @'[4, 2])

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

d333 = (dims @'[3,3,3])

t :: Tensor Int '[3, 3, 3] 
t = fill d333 $ majorToMinor d333 -- col major

t' :: Tensor Int '[3, 3, 3] 
t' = fill d333 $ minorToMajor d333 -- row major

res :: Tensor Int '[3, 3, 3] 
res = transpose re t'

transpose re t' == reshape t && transpose re t == reshape t'

-}
 

