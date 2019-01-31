{-# LANGUAGE FlexibleInstances, KindSignatures, MagicHash, TypeOperators, UnboxedSums, UnboxedTuples, UndecidableInstances #-}

module Data.Tensor.Types where

import Data.Bits
import Data.Singletons.Prelude.List (Product)
import Data.Int
import Data.Word

import GHC.TypeLits
import Numeric.Dimensions --(Dimensions(..), KnownDim(..), dimVal)

import Data.Vector.Primitive (Vector(..), Prim(..))
import qualified Data.Vector.Primitive as P


-- TODO: move to application / test stanza
type TVal = Float
type IVal = Word
type BVal = TVal

type Elt = Prim
--class Elt e
newtype Tensor (t :: *) (ds :: [Nat]) = Tensor { unTensor :: Vector t } deriving Eq

-- | A real or complex-valued tensor of shape 'd'. 
type T d = Tensor TVal d

-- | An integer or non-negative integer-valued tensor of shape 'd'. 
type I d = Tensor IVal d

-- | A boolean-valued tensor of shape 'd'. 
type B d = Tensor BVal d

instance (Num t, Prim t) => Num (Tensor t ds)  where
    {-# SPECIALIZE instance Num (Tensor Float ds)  #-}
    {-# SPECIALIZE instance Num (Tensor Double ds) #-}
    {-# SPECIALIZE instance Num (Tensor Int ds)    #-}
    {-# SPECIALIZE instance Num (Tensor Word ds)   #-}
    {-# SPECIALIZE instance Num (Tensor Int8 ds)   #-}
    {-# SPECIALIZE instance Num (Tensor Int16 ds)  #-}
    {-# SPECIALIZE instance Num (Tensor Int32 ds)  #-}
    {-# SPECIALIZE instance Num (Tensor Int64 ds)  #-}
    {-# SPECIALIZE instance Num (Tensor Word8 ds)  #-}
    {-# SPECIALIZE instance Num (Tensor Word16 ds) #-}
    {-# SPECIALIZE instance Num (Tensor Word32 ds) #-}
    {-# SPECIALIZE instance Num (Tensor Word64 ds) #-}
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
    fromInteger i = Tensor $ P.singleton (fromInteger i) --TODO make this dim safe
    {-# INLINE fromInteger #-}

instance (Fractional t, Prim t) => Fractional (Tensor t ds)  where
    {-# SPECIALIZE instance Fractional (Tensor Float ds)  #-}
    {-# SPECIALIZE instance Fractional (Tensor Double ds) #-}
    (/) = liftT2 (/)
    {-# INLINE (/) #-}
    recip = liftT recip
    {-# INLINE recip #-}
    fromRational r = Tensor $ P.singleton (fromRational r) --TODO make this dim safe
    {-# INLINE fromRational #-}


instance (Floating t, Prim t) => Floating (Tensor t ds) where
    {-# SPECIALIZE instance Floating (Tensor Float ds)  #-}
    {-# SPECIALIZE instance Floating (Tensor Double ds) #-}
    pi = Tensor $ P.singleton pi  --TODO make this dim safe
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


instance (KnownDim (Product d), Elt e, Eq e, Bits e, Num e) => Bits (Tensor e d) where
    (.&.) = liftT2 (.&.)
    {-# INLINE (.&.) #-}
    (.|.) = liftT2 (.|.)
    {-# INLINE (.|.) #-}
    xor = liftT2 xor
    {-# INLINE xor #-}
    complement = liftT complement
    shift t i = liftT (flip shift i) t
    rotate t i = liftT (flip rotate i) t
    bit = replicateT (fromIntegral . dimVal $ (dim :: Dim (Product d))) . bit
    testBit = testBitDefault
    bitSizeMaybe _ = bitSizeMaybe @e undefined
    bitSize _ = bitSize @e undefined
    isSigned _ = isSigned @e undefined
    popCount = popCountDefault

{-

instance (KnownDim (Product d), Elt e, Real e) => Real (Tensor e d) where
  toRational = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

instance (KnownDim (Product d), Elt e, Enum e) => Enum (Tensor e d) where
  toEnum = Tensor . V.replicate (fromIntegral . dimVal $ (dim :: Dim (Product d))) . toEnum
  {-# INLINE toEnum #-}
  fromEnum = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

instance (KnownDim (Product d), Elt e, Integral e) => Integral (Tensor e d) where
  quot (Tensor a) (Tensor b) = liftT2 quot a b
  rem (Tensor a) (Tensor b) = liftT2 rem a b
  div (Tensor a) (Tensor b) = liftT2 div a b
  mod (Tensor a) (Tensor b) = liftT2 mod a b
  quotRem ta tb = (quot ta tb, rem ta tb)
  divMod ta tb = (div ta tb, mod ta tb)
  toInteger _ = undefined --TODO find a reasonable sum-based implementation or scrap the typeclass

-}

constant
  :: forall d e. Elt e
  => KnownDim (Product d)
  => [e]
  -> Maybe (Tensor e d)
constant v
  | length v == fromIntegral (dimVal (dim :: Dim (Product d))) = Just $ Tensor $ P.fromListN (length v) v
  | otherwise = Nothing

{-



equal
  :: Elt e
  => Eq e
  => Tensor e d
  -> Tensor e d
  -> Tensor BVal d
equal = liftT2 (==)

notEqual
  :: Elt e
  => Eq e
  => Tensor e d
  -> Tensor e d
  -> Tensor BVal d
notEqual = liftT2 (/=)

-}

eq :: T d -> T d -> B d
eq = liftT2 $ (.)(.)(.) coerced (==)

neq :: T d -> T d -> B d
neq = liftT2 $ (.)(.)(.) coerced (/=)

lt :: Ord TVal => T d -> T d -> B d
lt = liftT2 $ (.)(.)(.) coerced (<)

lte :: Ord TVal => T d -> T d -> B d
lte = liftT2 $ (.)(.)(.) coerced (<=)

gt :: Ord TVal => T d -> T d -> B d
gt = liftT2 $ (.)(.)(.) coerced (>)

gte :: Ord TVal => T d -> T d -> B d
gte = liftT2 $ (.)(.)(.) coerced (>=)

maximum
  :: Elt e
  => Ord e
  => Tensor e d
  -> Tensor e d
  -> Tensor e d
maximum = liftT2 max

minimum
  :: Elt e
  => Ord e
  => Tensor e d
  -> Tensor e d
  -> Tensor e d
minimum = liftT2 min

--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------

coerced :: (Num t, Prim t) => Bool -> t
coerced True = 1
coerced False = 0

replicateT :: (Prim t, KnownDim (Product ds)) => Int -> t -> Tensor t ds
replicateT i = Tensor . P.fromListN i . replicate i

liftT :: (Prim s, Prim t) => (s -> t) -> Tensor s ds -> Tensor t ds
liftT f (Tensor v) = Tensor $ P.map f v
{-# INLINE liftT #-}

liftT2 :: (Prim r, Prim s, Prim t) => (r -> s -> t)
     -> Tensor r ds -> Tensor s ds -> Tensor t ds
liftT2 f (Tensor v1) (Tensor v2) = Tensor $ P.zipWith f v1 v2
{-# INLINE liftT2 #-}
