{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Numeric.Tensile.Tensor.Internal where

import Control.Monad.ST (ST(..))
import Data.Bits
import Data.Int
import Data.Kind
import Data.Proxy
import Data.Word
import Data.Vector.Storable (Storable(..))
import Unsafe.Coerce (unsafeCoerce)

import Numeric.Tensile.Dimensions

import Data.Vector.Sized (Vector)
import qualified Data.Finite as F
import qualified Data.Vector.Sized as N
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as M


-- TODO: move to application / test stanza
type Elt = Storable
type TVal = Float
type IVal = Word
type BVal = Bool

--class Elt e
--TODO update Show instance
newtype Tensor (d :: [Nat]) (e :: Type) = Tensor { unTensor :: S.Vector e } deriving (Eq, Show)

-- | A real or complex-valued tensor of shape 'd'. 
type T d = Tensor d TVal

-- | An integer or non-negative integer-valued tensor of shape 'd'. 
type I d = Tensor d IVal

-- | A boolean-valued tensor of shape 'd'. 
type B d = Tensor d BVal

instance (KnownDim (Size d), Num e, Elt e) => Num (Tensor d e)  where
    (+) = _lift2 (+)
    {-# INLINE (+) #-}
    (-) = _lift2 (-)
    {-# INLINE (-) #-}
    (*) = _lift2 (*)
    {-# INLINE (*) #-}
    negate = _lift negate
    {-# INLINE negate #-}
    abs = _lift abs
    {-# INLINE abs #-}
    signum = _lift signum
    {-# INLINE signum #-}
    fromInteger = _replicate (fromIntegral . dimVal $ (dim :: Dim (Size d))) . fromInteger
    {-# INLINE fromInteger #-}

instance (KnownDim (Size d), Fractional e, Elt e) => Fractional (Tensor d e)  where
    (/) = _lift2 (/)
    {-# INLINE (/) #-}
    recip = _lift recip
    {-# INLINE recip #-}
    fromRational = _replicate (fromIntegral . dimVal $ (dim :: Dim (Size d))) . fromRational
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
    bit = _replicate (fromIntegral . dimVal $ (dim :: Dim (Size d))) . bit
    testBit = testBitDefault
    bitSizeMaybe _ = bitSizeMaybe @e undefined
    bitSize _ = bitSize @e undefined
    isSigned _ = isSigned @e undefined
    popCount = popCountDefault

{-

instance (KnownDim (Size d), Elt e, Real e) => Real (Tensor d e) where
  toRational = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

instance (KnownDim (Size d), Elt e, Enum e) => Enum (Tensor d e) where
  toEnum = Tensor . S.replicate (fromIntegral . dimVal $ (dim :: Dim (Size d))) . toEnum
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


{-



equal
  :: Elt e
  => Eq t
  => Tensor d e
  -> Tensor d e
  -> Tensor BVal d
equal = _lift2 (==)

notEqual
  :: Elt e
  => Eq t
  => Tensor d e
  -> Tensor d e
  -> Tensor BVal d
notEqual = _lift2 (/=)

-}

eq :: T d -> T d -> B d
eq = _lift2 (==)

neq :: T d -> T d -> B d
neq = _lift2 (/=)

lt :: Ord TVal => T d -> T d -> B d
lt = _lift2 (<)

lte :: Ord TVal => T d -> T d -> B d
lte = _lift2 (<=)

gt :: Ord TVal => T d -> T d -> B d
gt = _lift2 (>)

gte :: Ord TVal => T d -> T d -> B d
gte = _lift2 (>=)

maximum
  :: Elt e
  => Ord e
  => Tensor d e
  -> Tensor d e
  -> Tensor d e
maximum = _lift2 max

minimum
  :: Elt e
  => Ord e
  => Tensor d e
  -> Tensor d e
  -> Tensor d e
minimum = _lift2 min

reshape 
  :: forall d d' e. Elt e 
  => Reshapable d d'
  => Tensor d e -> Tensor d' e
reshape = unsafeCoerce

fromList
  :: forall d e. Elt e
  => KnownDim (Size d)
  => [e]
  -> Maybe (Tensor d e)
fromList v
  | length v == fromIntegral (dimVal (dim @(Size d))) = Just $ Tensor $ S.fromListN (length v) v
  | otherwise = Nothing

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
  mv <- M.new $ fromIntegral $ product $ listDims d
  let act ix = M.write mv (minorToMajor d ix) $ f ix
  forMIdxs_ d act
  return mv

constant :: Elt e => Dims d -> e -> Tensor d e
constant d t = fill d $ const t

-- TODO unsafe consider using sized vectors internally
modifyIdxs :: forall d e. Storable e => Dims d -> S.Vector e -> (forall s. Idxs d -> M.MVector s e -> ST s ()) -> S.Vector e
modifyIdxs d v f = S.modify (\mv -> forMIdxs_ d (\i -> f i mv)) v

{-

fill :: Elt e => Dims d -> (Int -> e) -> Vector e
fill dims act = 
  case dims of 
    Reverse dims' -> fill' dims act
   
fill' :: Elt e => Dims d -> (Int -> e) -> Vector e
fill' dims act = S.create $ do
  v <- M.new (fromIntegral $ totalDim dims)
  let f i = M.write v i $ act i
  overDimOff_ dims f 0 1 -- either control the offset ourselves or fix type issues
  return v

TODO add tests:
> mod d idxs m = M.swap m 0 (fromIdxs d idxs)
> v = S.fromList [1..12]
> v
[1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0]
> modifyIdxs (dims @'[2, 2, 3]) v $ mod (dims @'[2, 2, 3])
[12.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0]

> modifyIdxs (dims @'[2, 2, 3]) v (\_ m -> M.set m 0)
[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]

-}

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
        n = dimVal $ dim @n
        size = product $ listDims d
        res = S.create $ do
          mv <- M.new $ size * n
          flip N.imapM_ v $ \i t -> 
            let i' = idxVal . idxFromFinite $ i
                off = i' * size
                v' = unTensor t
                act ix = M.write mv (off + fromEnum ix) $ v' S.! (fromEnum ix) -- could use a tensor op instead here
            in forMIdxs_ d act
          return mv


unpack0 
  :: forall d e n. Elt e
  => KnownDims d
  => KnownNat n
  => Tensor (n :+ d) e -> Vector n (Tensor d e)
unpack0 t = N.generate f
  where d = dims @d
        size = fromIntegral $ product $ listDims d
        f i = fill d $ \ix -> 
          let i' = fromIntegral $ F.getFinite i
              off = i' * size
              v = unTensor t 
          in v S.! (off + fromEnum ix)


{-

see example http://jeankossaifi.com/blog/unfolding.html

t :: Vector 4 (Tensor '[2,2] Word)
t = N.generate $ \f -> 
  let d = dims @'[2,2]
      i' = idxVal . idxFromFinite $ f
  in fill d (const i') 

t' :: Tensor '[4,2,2] Word
t' = pack0 t

t'' :: Vector 4 (Tensor '[2,2] Word)
t'' = unpack0 t'

generate :: forall n a. KnownNat n => (Finite n -> a) -> Vector n a

t :: Data.Vector.Sized.Vector 4 (Tensor '[2,2] Word)
t = generate $ \f -> 
  let d = dims @'[2,2]
      i' = idxVal . idxFromFinite $ f
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


--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------

_replicate :: Elt e => Int -> e -> Tensor d e
_replicate i = Tensor . S.fromListN i . replicate i

_lift :: (Elt a, Elt b) => (a -> b) -> Tensor d a -> Tensor d b
_lift f (Tensor v) = Tensor $ S.map f v
{-# INLINE _lift #-}

_lift2 :: (Elt a, Elt b, Elt c) => (a -> b -> c)
       -> Tensor d a -> Tensor d b -> Tensor d c
_lift2 f (Tensor v1) (Tensor v2) = Tensor $ S.zipWith f v1 v2
{-# INLINE _lift2 #-}
