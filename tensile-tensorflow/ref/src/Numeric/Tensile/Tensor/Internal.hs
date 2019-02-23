{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Numeric.Tensile.Tensor.Internal where

import Control.Monad.ST (ST(..))
import Data.Bits
import Data.ByteString (ByteString())
import Data.Complex
import Data.Int
import Data.Kind
import Data.Proxy
import Data.Word
import Data.Vector.Storable (Storable(..))
import Unsafe.Coerce (unsafeCoerce)

import Numeric.Tensile.Dimensions

import Data.Vector.Sized (Vector)
import TensorFlow.Types
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Finite as F
import qualified Data.Vector as V
import qualified Data.Vector.Sized as N
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as M
import qualified TensorFlow.Tensor as T
import qualified TensorFlow.Build as T
import qualified TensorFlow.GenOps.Core as O
import qualified TensorFlow.Ops as O
import qualified TensorFlow.Session as Ss

import Control.Monad.IO.Class (liftIO)



{-
fromTensor :: Elt e => Tensor d e -> [e]
fromTensor t = unsafePerformIO $ Ss.runSession $ do
  a <- T.render $ unTensor t
  b <- Ss.run a
  return $ V.toList b

:: OneOf '[Int32, Int64] tidx	 
=> Tensor v'1 Bool	
input

-> Tensor v'2 tidx	
reduction_indices

-> Tensor Build Bool

transpose 
  :: Elt e 
  => Permutable d d'
  => Dims d -> Perm (Rank d) -> Tensor d e -> Tensor d' e
transpose d (Perm p) (Tensor t) = Tensor $ (flip O.transpose) (O.vector w) t
  where v = [1.. fromIntegral $ rank d] :: [IVal]
        w = P.permuteList p v


-}
-- | Generally specialized as in 'TVal' or 'IVal'.
type Elt e = OneOf '[Int32, Int64, Word32, Word64, Float, Double, Complex Float, Complex Double] e

-- TODO: move to application / test stanza
--type Elt = TensorType -- TODO deal with Variant and exclude ResourceHandle
type TVal = Float
type IVal = Int32
type BVal = Bool


newtype Tensor (d :: [Nat]) (e :: Type) = Tensor { unTensor :: T.Tensor T.Build e }

-- | A real or complex-valued tensor of shape 'd'. 
type T d = Tensor d TVal

-- | An integer or non-negative integer-valued tensor of shape 'd'. 
type I d = Tensor d IVal

-- | A boolean-valued tensor of shape 'd'. 
type B d = Tensor d BVal

--TODO update Show instance
instance (Elt e, Show e) => Show (Tensor d e) where show _ = "No Show instance."



--TODO implement
instance (KnownDims d, Elt e, Eq e) => Eq (Tensor d e) where
    (==) ta tb = all $ ta `eq` tb
      where all :: B d -> Bool
            all (Tensor b) = Prelude.head $ toList $ Tensor $ O.all b (O.vector ([] :: [IVal]))



instance (KnownDims d, Num e, Elt e) => Num (Tensor d e) where
    (+) (Tensor v1) (Tensor v2) = Tensor $ (+) v1 v2
    {-# INLINE (+) #-}
    (-) (Tensor v1) (Tensor v2) = Tensor $ (-) v1 v2
    {-# INLINE (-) #-}
    (*) (Tensor v1) (Tensor v2) = Tensor $ (*) v1 v2
    {-# INLINE (*) #-}
    negate (Tensor v1) = Tensor $ negate v1
    {-# INLINE negate #-}
    abs (Tensor v1) = Tensor $ abs v1
    {-# INLINE abs #-}
    signum (Tensor v1) = Tensor $ signum v1
    {-# INLINE signum #-}
    fromInteger = constant (dims @d) . fromInteger
    {-# INLINE fromInteger #-}

{-
instance (KnownDim (Size d), Fractional e, Elt e) => Fractional (Tensor d e)  where
    (/) = _lift2 (/)
    {-# INLINE (/) #-}
    recip = _lift recip
    {-# INLINE recip #-}
    fromRational = _replicate (fromIntegral . fromDim $ (dim :: Dim (Size d))) . fromRational
    {-# INLINE fromRational #-}

instance (KnownDim (Size d), Floating e, Elt e) => Floating (Tensor d e) where
    pi = Tensor $ S.singleton pi  --TODO make this dim safe
    {-# INLINE pi #-}
    exp = _lift exp
    {-# INLINE exp #-}
    log = _lift log
    {-# INLINE log #-}
    sqrt = _lift sqrt
    {-# INLINE sqrt #-}
    sin = _lift sin
    {-# INLINE sin #-}
    cos = _lift cos
    {-# INLINE cos #-}
    tan = _lift tan
    {-# INLINE tan #-}
    asin = _lift asin
    {-# INLINE asin #-}
    acos = _lift acos
    {-# INLINE acos #-}
    atan = _lift atan
    {-# INLINE atan #-}
    sinh = _lift sinh
    {-# INLINE sinh #-}
    cosh = _lift cosh
    {-# INLINE cosh #-}
    tanh = _lift tanh
    {-# INLINE tanh #-}
    (**) = _lift2 (**)
    {-# INLINE (**) #-}
    logBase = _lift2 logBase
    {-# INLINE logBase #-}
    asinh = _lift asinh
    {-# INLINE asinh #-}
    acosh = _lift acosh
    {-# INLINE acosh #-}
    atanh = _lift atanh
    {-# INLINE atanh #-}

instance (KnownDim (Size d), Elt e, Eq e, Bits e, Num e) => Bits (Tensor d e) where
    (.&.) = _lift2 (.&.)
    {-# INLINE (.&.) #-}
    (.|.) = _lift2 (.|.)
    {-# INLINE (.|.) #-}
    xor = _lift2 xor
    {-# INLINE xor #-}
    complement = _lift complement
    shift t i = _lift (flip shift i) t
    rotate t i = _lift (flip rotate i) t
    bit = _replicate (fromIntegral . fromDim $ (dim :: Dim (Size d))) . bit
    testBit = testBitDefault
    bitSizeMaybe _ = bitSizeMaybe @e undefined
    bitSize _ = bitSize @e undefined
    isSigned _ = isSigned @e undefined
    popCount = popCountDefault
-}

{-

instance (KnownDim (Size d), Elt e, Real e) => Real (Tensor d e) where
  toRational = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

instance (KnownDim (Size d), Elt e, Enum e) => Enum (Tensor d e) where
  toEnum = Tensor . S.replicate (fromIntegral . fromDim $ (dim :: Dim (Size d))) . toEnum
  {-# INLINE toEnum #-}
  fromEnum = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

instance (KnownDim (Size d), Elt e, Integral e) => Integral (Tensor d e) where
  quot (Tensor a) (Tensor b) = _lift2 quot a b
  rem (Tensor a) (Tensor b) = _lift2 rem a b
  div (Tensor a) (Tensor b) = _lift2 div a b
  mod (Tensor a) (Tensor b) = _lift2 mod a b
  quotRem ta tb = (quot ta tb, rem ta tb)
  divMod ta tb = (div ta tb, mod ta tb)
  toInteger _ = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

-}


eq :: Elt e => Eq e => Tensor d e -> Tensor d e -> Tensor d BVal
eq (Tensor a) (Tensor b) = Tensor $ O.equal a b

neq :: Elt e => Eq e => Tensor d e -> Tensor d e -> Tensor d BVal
neq (Tensor a) (Tensor b) = Tensor $ O.notEqual a b

lt :: Elt e => Ord e => Tensor d e -> Tensor d e -> Tensor d BVal
--lt :: Ord TVal => T d -> T d -> B d
lt (Tensor a) (Tensor b) = Tensor $ O.less a b

lte :: Elt e => Ord e => Tensor d e -> Tensor d e -> Tensor d BVal
lte (Tensor a) (Tensor b) = Tensor $ O.lessEqual a b

gt :: Elt e => Ord e => Tensor d e -> Tensor d e -> Tensor d BVal
gt (Tensor a) (Tensor b) = Tensor $ O.greater a b

gte :: Elt e => Ord e => Tensor d e -> Tensor d e -> Tensor d BVal
gte (Tensor a) (Tensor b) = Tensor $ O.greaterEqual a b

maximum :: Elt e => Ord e => Tensor d e -> Tensor d e -> Tensor d e
maximum (Tensor a) (Tensor b) = Tensor $ O.maximum a b

minimum :: Elt e => Ord e => Tensor d e -> Tensor d e -> Tensor d e
minimum (Tensor a) (Tensor b) = Tensor $ O.minimum a b

fromList' :: forall d e. Elt e => KnownDims d => [e] -> Maybe (Tensor d e)
fromList' l = reflectDims @d $ \d -> fromList d l

fromList :: forall d e. Elt e => Dims d -> [e] -> Maybe (Tensor d e)
fromList d l = if size d == (fromIntegral $ length l) then Just $ f l else Nothing
  where f = Tensor . O.constant (toShape d)

toList :: TensorDataType V.Vector a => Tensor d a -> [a]
toList t = unsafePerformIO $ Ss.runSession $ do
  a <- T.render $ unTensor t
  b <- Ss.run a
  return $ V.toList b

{-

fromList :: forall d e. Elt e => KnownDims d => [e] -> Maybe (Tensor d e)
fromList v = if size (dims @d) == (fromIntegral $ length v) then Just $ f v else Nothing
  where f = Tensor . O.constant (toShape (dims @d))



-- fromVector :: Elt e => Dims d -> Vector e -> Maybe (Tensor d e)
-- fromVector d v = undefined
-- 
-- toVector :: Elt e => Tensor d e -> Vector e
-- toVector = unTensor
-- 
fromSizedVector :: Elt e => Vector (Size d) e -> Tensor d e
fromSizedVector = Tensor . S.convert . N.fromSized

toSizedVector :: Elt e => Tensor d e -> Vector (Size d) e
toSizedVector = coerce . S.convert . unTensor
  where coerce :: S.Vector e -> N.Vector n e
        coerce = unsafeCoerce

fill :: Elt e => Dims d -> (Idxs d -> e) -> Tensor d e
fill d f = Tensor $ S.create $ do
  mv <- M.new $ fromIntegral $ product $ fromDims d
  let act ix = M.write mv (minorToMajor d ix) $ f ix
  forMIdxs_ d act
  return mv

-- constant :: TensorType a => Shape -> [a] -> Tensor Build a

constant :: Elt e => Dims d -> e -> Tensor d e
constant d t = fill d $ const t

-}

constant :: Elt e => Dims d -> e -> Tensor d e
constant d = Tensor . O.constant (toShape d) . pure

toShape :: Dims d -> Shape
toShape = Shape . fmap fromIntegral . fromDims 

toShape' :: Idxs d -> Shape
toShape' = Shape . fmap fromIntegral . listIdxs 

toShapeTensor :: Idxs d -> I '[Rank d]
toShapeTensor i = Tensor . O.constant (toShape' i) $ l
  where l = fmap fromIntegral . listIdxs $ i

-- TODO use more efficient density list approach
fill :: Elt e => Dims d -> (Idxs d -> e) -> Tensor d e
fill d k = Tensor . O.constant (toShape d) $ Prelude.reverse l
  where l = foldIdxs d (\i x -> k i : x) []
        

{-

fromIntegral' :: Int -> Int32
fromIntegral' = fromIntegral

toList $ fill (dims @'[2,4]) (fromIntegral' . fromEnum)


t1 = fill (dims @'[2,4]) $ const 1 . fromEnum
t2 = fill (dims @'[4,2]) $ const 1 . fromEnum

t2 = fill (dims @'[2,4]) ((+1) . fromIntegral' . fromEnum)

f :: Elt e => Tensor '[2,4] e -> Tensor '[4,2] e
f = transpose (dims @'[2,4]) (reversal (dims @'[2,4]))

t1' = f t1



foo :: Dims d -> (Idxs d -> TVal) -> IO ()
foo d f = printTensor $ fill d f

> d = dims @'[2,4]
> foo d (minorToMajor d)
[7,6,5,4,3,2,1,0]
> foo d (transposeIdxs $ majorToMinor d)
[7,5,3,1,6,4,2,0]

pred_sum_idxs :: forall ds. Dims ds -> Bool
pred_sum_idxs ds = foldIdxs ds (\_ a -> a + 1) 0 == (product . fromDims $ ds)

-}

{-

Splits a tensor into sub tensors.

If num_or_size_splits is an integer type, then value is split along dimension axis into num_split smaller tensors. Requires that num_split evenly divides value.shape[axis].

If num_or_size_splits is not an integer type, it is presumed to be a Tensor size_splits, then splits value into len(size_splits) pieces. The shape of the i-th piece has the same size as the value except along dimension axis where the size is size_splits[i].


slice:
t = tf.constant([[[1, 1, 1], [2, 2, 2]],
                 [[3, 3, 3], [4, 4, 4]],
                 [[5, 5, 5], [6, 6, 6]]]) 3 x 2 x 3
tf.slice(t, [1, 0, 0], [1, 1, 3])  # [[[3, 3, 3]]]
tf.slice(t, [1, 0, 0], [1, 2, 3])  # [[[3, 3, 3],
                                   #   [4, 4, 4]]]
tf.slice(t, [1, 0, 0], [2, 1, 3])  # [[[3, 3, 3]],
                                   #  [[5, 5, 5]]]

-- want d' < d, 
slice :: Idxs d -> Dims d' -> Tensor d e -> Tensor d' e

slice :: (TensorType t, OneOf '[Int32, Int64] index)	=> Tensor v'1 t	-> Tensor v'2 index	-> Tensor v'3 index	-> Tensor Build t

Idxs d -> I '[Rank d]


i = toEnum 6 :: Idxs '[3,2,3]
j = toEnum 9 :: Idxs '[3,2,3]

diffIdxs (dims @'[3,2,3]) i j

stepIdx (dims @'[3,2,3]) 3 i
i = 1 :+ 0 :+ 0 :+ S :: Idxs '[3,2,3]


-}




{-

pack
  :: Elt e 
  => Vector n (Tensor (x ++ y) e) -> Tensor (x ++ n :+ y) e
pack = undefined

pack0
  :: forall d e n. Elt e
  => KnownDims d
  => KnownDim n
  => Vector n (Tensor d e) -> Tensor (n :+ d) e
pack0 v = Tensor res
  where d = dims @d
        n = fromIntegral $ fromDim $ dim @n
        size = product $ fromDims d
        res = S.create $ do
          mv <- M.new $ fromIntegral $ size * n
          flip N.imapM_ v $ \i t -> 
            let i' = idxToWord . idxFromFinite $ i
                off = fromIntegral $ i' * size
                v' = unTensor t
                act ix = M.write mv (off + fromEnum ix) $ v' S.! (fromEnum ix) -- could use a tensor op instead here
            in forMIdxs_ d act
          return mv

-- TODO use http://hackage.haskell.org/package/reflection-2.1.4/docs/Data-Reflection.html#t:reifyNat
unpack0 
  :: forall d e n. Elt e
  => KnownDims d
  => KnownNat n
  => Tensor (n :+ d) e -> Vector n (Tensor d e)
unpack0 t = N.generate f
  where d = dims @d
        size = fromIntegral $ product $ fromDims d
        f i = fill d $ \ix -> 
          let i' = fromIntegral $ F.getFinite i
              off = i' * size
              v = unTensor t 
          in v S.! (off + fromEnum ix)
-}

{-

see example http://jeankossaifi.com/blog/unfolding.html

t :: Vector 4 (Tensor '[2,2] Word)
t = N.generate $ \f -> 
  let d = dims @'[2,2]
      i' = idxToWord . idxFromFinite $ f
  in fill d (const i') 

t' :: Tensor '[4,2,2] Word
t' = pack0 t

t'' :: Vector 4 (Tensor '[2,2] Word)
t'' = unpack0 t'

generate :: forall n a. KnownNat n => (Finite n -> a) -> Vector n a

t :: Data.Vector.Sized.Vector 4 (Tensor '[2,2] Word)
t = generate $ \f -> 
  let d = dims @'[2,2]
      i' = idxToWord . idxFromFinite $ f
  in fill d (const i') 

t' :: Tensor '[4,2,2] Word
t' = pack0 t
N.imapM_ :: Monad m => (Finite n -> a -> m b) -> Vector n a -> m ()

overDim_ :: Monad m
         => Dims ds -- ^ Shape of a space
         -> (Idxs ds -> Int -> m ()) -- ^ Function to call on each dimension
         -> Int -- ^ Initial offset
         -> Int -- ^ Offset step
         -> m ()

stack ::  Dim n -> Vector n (Tensor (x ++ y) e) -> Tensor (x +: n :+ y) e
unstack :: (KnownDims x, Elt e) => Dim n -> Tensor (x +: n :+ y) e -> Vector n (Tensor (x ++ y) e)
-}


