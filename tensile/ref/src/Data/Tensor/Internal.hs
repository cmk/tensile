{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances #-}
module Data.Tensor.Internal where

import Control.Monad.ST (ST(..))
import Data.Bits
import Data.Int
import Data.Word
import Data.Vector.Storable (Vector(..), Storable(..))

import Numeric.Tensile.Types
import Numeric.Tensile.Index

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M


-- TODO: move to application / test stanza
type Elt = Storable
type TVal = Float
type IVal = Word
type BVal = Bool

--class Elt t
--TODO update Show instance
newtype Tensor (t :: *) (d :: [Nat]) = Tensor { unTensor :: Vector t } deriving (Eq, Show)

-- | A real or complex-valued tensor of shape 'd'. 
type T d = Tensor TVal d

-- | An integer or non-negative integer-valued tensor of shape 'd'. 
type I d = Tensor IVal d

-- | A boolean-valued tensor of shape 'd'. 
type B d = Tensor BVal d

instance (KnownDim (Size d), Num t, Elt t) => Num (Tensor t d)  where
    (+) = liftT2 (+)
    {-# INLINE (+) #-}
    (-) = liftT2 (-)
    {-# INLINE (-) #-}
    (*) = liftT2 (*)
    {-# INLINE (*) #-}
    negate = liftT negate
    {-# INLINE negate #-}
    abs = liftT abs
    {-# INLINE abs #-}
    signum = liftT signum
    {-# INLINE signum #-}
    fromInteger = replicateT (fromIntegral . dimVal $ (dim :: Dim (Size d))) . fromInteger
    {-# INLINE fromInteger #-}

instance (KnownDim (Size d), Fractional t, Elt t) => Fractional (Tensor t d)  where
    (/) = liftT2 (/)
    {-# INLINE (/) #-}
    recip = liftT recip
    {-# INLINE recip #-}
    fromRational = replicateT (fromIntegral . dimVal $ (dim :: Dim (Size d))) . fromRational
    {-# INLINE fromRational #-}

instance (KnownDim (Size d), Floating t, Elt t) => Floating (Tensor t d) where
    pi = Tensor $ V.singleton pi  --TODO make this dim safe
    {-# INLINE pi #-}
    exp = liftT exp
    {-# INLINE exp #-}
    log = liftT log
    {-# INLINE log #-}
    sqrt = liftT sqrt
    {-# INLINE sqrt #-}
    sin = liftT sin
    {-# INLINE sin #-}
    cos = liftT cos
    {-# INLINE cos #-}
    tan = liftT tan
    {-# INLINE tan #-}
    asin = liftT asin
    {-# INLINE asin #-}
    acos = liftT acos
    {-# INLINE acos #-}
    atan = liftT atan
    {-# INLINE atan #-}
    sinh = liftT sinh
    {-# INLINE sinh #-}
    cosh = liftT cosh
    {-# INLINE cosh #-}
    tanh = liftT tanh
    {-# INLINE tanh #-}
    (**) = liftT2 (**)
    {-# INLINE (**) #-}
    logBase = liftT2 logBase
    {-# INLINE logBase #-}
    asinh = liftT asinh
    {-# INLINE asinh #-}
    acosh = liftT acosh
    {-# INLINE acosh #-}
    atanh = liftT atanh
    {-# INLINE atanh #-}

instance (KnownDim (Size d), Elt t, Eq t, Bits t, Num t) => Bits (Tensor t d) where
    (.&.) = liftT2 (.&.)
    {-# INLINE (.&.) #-}
    (.|.) = liftT2 (.|.)
    {-# INLINE (.|.) #-}
    xor = liftT2 xor
    {-# INLINE xor #-}
    complement = liftT complement
    shift t i = liftT (flip shift i) t
    rotate t i = liftT (flip rotate i) t
    bit = replicateT (fromIntegral . dimVal $ (dim :: Dim (Size d))) . bit
    testBit = testBitDefault
    bitSizeMaybe _ = bitSizeMaybe @t undefined
    bitSize _ = bitSize @t undefined
    isSigned _ = isSigned @t undefined
    popCount = popCountDefault

{-

instance (KnownDim (Size d), Elt t, Real e) => Real (Tensor t d) where
  toRational = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

instance (KnownDim (Size d), Elt t, Enum e) => Enum (Tensor t d) where
  toEnum = Tensor . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Size d))) . toEnum
  {-# INLINE toEnum #-}
  fromEnum = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

instance (KnownDim (Size d), Elt t, Integral e) => Integral (Tensor t d) where
  quot (Tensor a) (Tensor b) = liftT2 quot a b
  rem (Tensor a) (Tensor b) = liftT2 rem a b
  div (Tensor a) (Tensor b) = liftT2 div a b
  mod (Tensor a) (Tensor b) = liftT2 mod a b
  quotRem ta tb = (quot ta tb, rem ta tb)
  divMod ta tb = (div ta tb, mod ta tb)
  toInteger _ = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

-}


{-



equal
  :: Elt t
  => Eq t
  => Tensor t d
  -> Tensor t d
  -> Tensor BVal d
equal = liftT2 (==)

notEqual
  :: Elt t
  => Eq t
  => Tensor t d
  -> Tensor t d
  -> Tensor BVal d
notEqual = liftT2 (/=)

-}

eq :: T d -> T d -> B d
eq = liftT2 (==)

neq :: T d -> T d -> B d
neq = liftT2 (/=)

lt :: Ord TVal => T d -> T d -> B d
lt = liftT2 (<)

lte :: Ord TVal => T d -> T d -> B d
lte = liftT2 (<=)

gt :: Ord TVal => T d -> T d -> B d
gt = liftT2 (>)

gte :: Ord TVal => T d -> T d -> B d
gte = liftT2 (>=)

maximum
  :: Elt t
  => Ord t
  => Tensor t d
  -> Tensor t d
  -> Tensor t d
maximum = liftT2 max

minimum
  :: Elt t
  => Ord t
  => Tensor t d
  -> Tensor t d
  -> Tensor t d
minimum = liftT2 min

{-
vector :: forall n . KnownDim n => KnownNat n => [HsReal] -> ExceptT String IO (Tensor '[n])
vector rs
  | genericLength rs == dimVal (dim :: Dim n) = asStatic <$> Dynamic.vectorEIO rs
  | otherwise = ExceptT . pure $ Left "Vector dimension does not match length of list"

unsafeVector :: (KnownDim n, KnownNat n) => [HsReal] -> IO (Tensor '[n])
unsafeVector = fmap (either error id) . runExceptT . vector
-}

toTensor
  :: forall d t. Elt t
  => KnownDim (Size d)
  => [t]
  -> Maybe (Tensor t d)
toTensor v
  | length v == fromIntegral (dimVal (dim :: Dim (Size d))) = Just $ Tensor $ V.fromListN (length v) v
  | otherwise = Nothing

fromVector :: Elt t => Dims d -> Vector t -> Maybe (Tensor t d)
fromVector d v = undefined

fromVector' :: forall d t. (Elt t, KnownDims d) => Vector t -> Maybe (Tensor t d)
fromVector' = fromVector (dims @_ @d)

fromScalar :: Elt t => t -> Tensor t '[]
fromScalar = constant (dims @_ @'[])

constant :: Elt t => Dims d -> t -> Tensor t d
constant d t = fill d $ const t

fill :: forall d t. Elt t => Dims d -> (Idxs d -> t) -> Tensor t d
fill d f = Tensor $ V.create $ do
  mv <- M.new (fromIntegral $ totalDim d)
  let act ix = M.write mv (minorToMajor d ix) $ f ix
  overDimIdx_ d act
  return mv

modifyIdxs :: forall t d. Storable t => Dims d -> Vector t -> (forall s. Idxs d -> M.MVector s t -> ST s ()) -> Vector t
modifyIdxs d v f = V.modify (\mv -> overDimIdx_ d (\i -> f i mv)) v

{-

fill :: Elt t => Dims d -> (Int -> t) -> Vector t
fill dims act = 
  case dims of 
    Reverse dims' -> fill' dims act
   
fill' :: Elt t => Dims d -> (Int -> t) -> Vector t
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

replicateT :: Elt t => Int -> t -> Tensor t d
replicateT i = Tensor . V.fromListN i . replicate i

liftT :: (Elt s, Elt t) => (s -> t) -> Tensor s d -> Tensor t d
liftT f (Tensor v) = Tensor $ V.map f v
{-# INLINE liftT #-}

liftT2 :: (Elt r, Elt s, Elt t) => (r -> s -> t)
       -> Tensor r d -> Tensor s d -> Tensor t d
liftT2 f (Tensor v1) (Tensor v2) = Tensor $ V.zipWith f v1 v2
{-# INLINE liftT2 #-}
