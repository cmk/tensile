{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.Tensile.Tensor.Internal where

import Control.Monad.ST (ST(..))
import Data.Bits
import Data.Int
import Data.Kind
import Data.Word
import Data.Vector.Storable (Vector(..), Storable(..))
import Unsafe.Coerce (unsafeCoerce)

import Numeric.Tensile.Types
import Numeric.Tensile.Index

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M


-- TODO: move to application / test stanza
type Elt = Storable
type TVal = Float
type IVal = Word
type BVal = Bool

--class Elt e
--TODO update Show instance
newtype Tensor (d :: [Pos]) (e :: Type) = Tensor { unTensor :: Vector e } deriving (Eq, Show)

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
    pi = Tensor $ V.singleton pi  --TODO make this dim safe
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
  toEnum = Tensor . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Size d))) . toEnum
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

{-
vector :: forall n . KnownDim n => KnownNat n => [HsReal] -> ExceptT String IO (Tensor '[n])
vector rs
  | genericLength rs == dimVal (dim :: Dim n) = asStatic <$> Dynamic.vectorEIO rs
  | otherwise = ExceptT . pure $ Left "Vector dimension does not match length of list"

unsafeVector :: (KnownDim n, KnownNat n) => [HsReal] -> IO (Tensor '[n])
unsafeVector = fmap (either error id) . runExceptT . vector
-}

reshape 
  :: forall d d' e. Elt e 
  => Reshapable d d'
  => Tensor d e -> Tensor d' e
reshape = unsafeCoerce

toTensor
  :: forall d e. Elt e
  => KnownDim (Size d)
  => [e]
  -> Maybe (Tensor d e)
toTensor v
  | length v == fromIntegral (dimVal (dim :: Dim (Size d))) = Just $ Tensor $ V.fromListN (length v) v
  | otherwise = Nothing

toVector :: Elt e => Tensor d e -> Vector e
toVector = unTensor

fromVector :: Elt e => Dims d -> Vector e -> Maybe (Tensor d e)
fromVector d v = undefined

fill :: forall d e. Elt e => Dims d -> (Idxs d -> e) -> Tensor d e
fill d f = Tensor $ V.create $ do
  mv <- M.new $ fromIntegral $ product $ listDims d
  let act ix = M.write mv (minorToMajor d ix) $ f ix
  overDimIdx_ d act
  return mv

-- TODO unsafe consider using sized vectors internally
modifyIdxs :: forall d e. Storable e => Dims d -> Vector e -> (forall s. Idxs d -> M.MVector s e -> ST s ()) -> Vector e
modifyIdxs d v f = V.modify (\mv -> overDimIdx_ d (\i -> f i mv)) v

{-

fill :: Elt e => Dims d -> (Int -> e) -> Vector e
fill dims act = 
  case dims of 
    Reverse dims' -> fill' dims act
   
fill' :: Elt e => Dims d -> (Int -> e) -> Vector e
fill' dims act = V.create $ do
  v <- M.new (fromIntegral $ totalDim dims)
  let f i = M.write v i $ act i
  overDimOff_ dims f 0 1 -- either control the offset ourselves or fix type issues
  return v

TODO add tests:
> mod d idxs m = M.swap m 0 (fromIdxs d idxs)
> v = V.fromList [1..12]
> v
[1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0]
> modifyIdxs (dims @_ @'[2, 2, 3]) v $ mod (dims @_ @'[2, 2, 3])
[12.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0]

> modifyIdxs (dims @_ @'[2, 2, 3]) v (\_ m -> M.set m 0)
[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]

-}


--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------

_replicate :: Elt e => Int -> e -> Tensor d e
_replicate i = Tensor . V.fromListN i . replicate i

_lift :: (Elt a, Elt b) => (a -> b) -> Tensor d a -> Tensor d b
_lift f (Tensor v) = Tensor $ V.map f v
{-# INLINE _lift #-}

_lift2 :: (Elt a, Elt b, Elt c) => (a -> b -> c)
       -> Tensor d a -> Tensor d b -> Tensor d c
_lift2 f (Tensor v1) (Tensor v2) = Tensor $ V.zipWith f v1 v2
{-# INLINE _lift2 #-}
