{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances #-}
module Data.Tensor.Types where

import Data.Bits
import Data.Int
import Data.Word

import Numeric.Tensile.Types

import Data.Vector.Storable (Vector(..), Storable(..))
import qualified Data.Vector.Storable as V


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

instance (Num t, Elt t) => Num (Tensor t d)  where
    {-# SPECIALIZE instance Num (Tensor Float d)  #-}
    {-# SPECIALIZE instance Num (Tensor Double d) #-}
    {-# SPECIALIZE instance Num (Tensor Int d)    #-}
    {-# SPECIALIZE instance Num (Tensor Word d)   #-}
    {-# SPECIALIZE instance Num (Tensor Int8 d)   #-}
    {-# SPECIALIZE instance Num (Tensor Int16 d)  #-}
    {-# SPECIALIZE instance Num (Tensor Int32 d)  #-}
    {-# SPECIALIZE instance Num (Tensor Int64 d)  #-}
    {-# SPECIALIZE instance Num (Tensor Word8 d)  #-}
    {-# SPECIALIZE instance Num (Tensor Word16 d) #-}
    {-# SPECIALIZE instance Num (Tensor Word32 d) #-}
    {-# SPECIALIZE instance Num (Tensor Word64 d) #-}
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
    fromInteger i = Tensor $ V.singleton (fromInteger i) --TODO make this dim safe
    {-# INLINE fromInteger #-}

instance (Fractional t, Elt t) => Fractional (Tensor t d)  where
    {-# SPECIALIZE instance Fractional (Tensor Float d)  #-}
    {-# SPECIALIZE instance Fractional (Tensor Double d) #-}
    (/) = liftT2 (/)
    {-# INLINE (/) #-}
    recip = liftT recip
    {-# INLINE recip #-}
    fromRational r = Tensor $ V.singleton (fromRational r) --TODO make this dim safe
    {-# INLINE fromRational #-}


instance (Floating t, Elt t) => Floating (Tensor t d) where
    {-# SPECIALIZE instance Floating (Tensor Float d)  #-}
    {-# SPECIALIZE instance Floating (Tensor Double d) #-}
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

toTensor
  :: forall d t. Elt t
  => KnownDim (Size d)
  => [t]
  -> Maybe (Tensor t d)
toTensor v
  | length v == fromIntegral (dimVal (dim :: Dim (Size d))) = Just $ Tensor $ V.fromListN (length v) v
  | otherwise = Nothing

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

--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------

replicateT :: (Elt t, KnownDim (Size d)) => Int -> t -> Tensor t d
replicateT i = Tensor . V.fromListN i . replicate i

liftT :: (Elt s, Elt t) => (s -> t) -> Tensor s d -> Tensor t d
liftT f (Tensor v) = Tensor $ V.map f v
{-# INLINE liftT #-}

liftT2 :: (Elt r, Elt s, Elt t) => (r -> s -> t)
       -> Tensor r d -> Tensor s d -> Tensor t d
liftT2 f (Tensor v1) (Tensor v2) = Tensor $ V.zipWith f v1 v2
{-# INLINE liftT2 #-}
