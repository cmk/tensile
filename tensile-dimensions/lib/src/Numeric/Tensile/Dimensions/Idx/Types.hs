{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE ExplicitNamespaces         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MagicHash                  #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TypeApplications           #-}
#if __GLASGOW_HASKELL__ >= 802
#else
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
#endif

module Numeric.Tensile.Dimensions.Idx.Types where


import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Finite as F
import qualified Math.Combinat.Permutations as P


import Data.Function (on)

import Data.Proxy
import GHC.TypeLits (KnownNat(..))
import Control.Arrow           (first)
import GHC.Base
import GHC.Enum

import Numeric.Tensile.Dimensions.Dim.Types
import Numeric.Tensile.Dimensions.Types as T

--TODO hide constructor
newtype Idx (d :: Nat) = Idx Int deriving (Eq, Ord)

idx :: Integral i => Dim d -> i -> Idx d
idx d = Idx . flip mod (dimVal d) . fromIntegral

idxVal :: Num n => Idx d -> n
idxVal (Idx i) = fromIntegral i
{-# INLINE idxVal #-}

liftIdx :: Dim d -> (Int -> Int) -> Idx d -> Idx d
liftIdx d f = idx d . f . idxVal

liftIdx2 :: Dim d -> (Int -> Int -> Int) -> Idx d -> Idx d -> Idx d
liftIdx2 d f = on k idxVal
  where k i j = idx d $ f i j

idxFromFinite :: forall d. KnownDim d => F.Finite d -> Idx d
idxFromFinite = reflectDim @d idx . F.getFinite

finiteFromIdx :: forall d. KnownNat d => Idx d -> F.Finite d
finiteFromIdx = F.finite . toInteger . idxVal 

instance Read (Idx d) where
    readsPrec d = fmap (first Idx) . readsPrec d

instance Show (Idx d) where
    showsPrec d = showsPrec d . idxVal

instance KnownDim d => Bounded (Idx d) where
    minBound = reflectDim @d idx 0
    {-# INLINE minBound #-}
    maxBound = reflectDim @d idx $ reflectDim @d dimVal - 1
    {-# INLINE maxBound #-}

instance KnownDim d => Enum (Idx d) where

    succ = reflectDim @d liftIdx (+1)
    {-# INLINE succ #-}

    pred = reflectDim @d liftIdx (+(-1))
    {-# INLINE pred #-}

    toEnum = reflectDim @d idx
    {-# INLINE toEnum #-}

    fromEnum = idxVal
    {-# INLINE fromEnum #-}

{-
    enumFrom (Idx n)
      = unsafeCoerce# (enumFromTo n (unsafeCoerce# (dim @d)))
    {-# INLINE enumFrom #-}
    enumFromThen (Idx n0) (Idx n1)
      = case compare n0 n1 of
          LT -> unsafeCoerce# (enumFromThenTo n0 n1 (unsafeCoerce# (dim @d)))
          EQ -> unsafeCoerce# (repeat n0)
          GT -> unsafeCoerce# (enumFromThenTo n0 n1 1)
    {-# INLINE enumFromThen #-}
    enumFromTo
      = unsafeCoerce# (enumFromTo :: Int -> Int -> [Int])
    {-# INLINE enumFromTo #-}
    enumFromThenTo
      = unsafeCoerce# (enumFromThenTo :: Int -> Int -> Int -> [Int])
    {-# INLINE enumFromThenTo #-}
-}

{-
class (Real a, Enum a) => Integral a where
    quot :: a -> a -> a
    rem :: a -> a -> a
    div :: a -> a -> a
    mod :: a -> a -> a
    quotRem :: a -> a -> (a, a)
    divMod :: a -> a -> (a, a)
    toInteger :: a -> Integer
    {-# MINIMAL quotRem, toInteger #-}
-}

instance KnownDim d => Num (Idx d) where

    (+) = reflectDim @d liftIdx2 (+)
    {-# INLINE (+) #-}

    (-) = reflectDim @d liftIdx2 (-)
    {-# INLINE (-) #-}

    (*) = reflectDim @d liftIdx2 (*)
    {-# INLINE (*) #-}

    signum = reflectDim @d liftIdx signum
    {-# INLINE signum #-}

    abs = id 
    {-# INLINE abs #-}

    fromInteger = reflectDim @d idx
    {-# INLINE fromInteger #-}


{-
instance KnownDim d => Num (Idx d) where

#ifdef UNSAFE_INDICES
    (+) = unsafeCoerce ((+) :: Int -> Int -> Int)
#else
    (Idx a) + (Idx b)
        | r >= d || r < a || r < b
          = errorWithoutStackTrace
          $ "Num.(+){Idx " ++ show d ++ "}: sum of "
            ++ show a ++ " and " ++ show b
            ++ " is outside of index bounds."
        | otherwise = Idx r
      where
        r = a + b
        d = unsafeCoerce (dim @d)
#endif
    {-# INLINE (+) #-}

#ifdef UNSAFE_INDICES
    (-) = unsafeCoerce ((-) :: Int -> Int -> Int)
#else
    (Idx a) - (Idx b)
        | b > a
          = errorWithoutStackTrace
          $ "Num.(-){Idx " ++ show (dim @d) ++ "}: difference of "
            ++ show a ++ " and " ++ show b
            ++ " is negative."
        | otherwise = Idx (a - b)
#endif
    {-# INLINE (-) #-}

#ifdef UNSAFE_INDICES
    (*) = unsafeCoerce ((*) :: Int -> Int -> Int)
#else
    (Idx a) * (Idx b)
        | r >= d || r < a || r < b
          = errorWithoutStackTrace
          $ "Num.(*){Idx " ++ show d ++ "}: product of "
            ++ show a ++ " and " ++ show b
            ++ " is outside of index bounds."
        | otherwise = Idx r
      where
        r = a * b
        d = unsafeCoerce (dim @d)
#endif
    {-# INLINE (*) #-}

    negate = errorWithoutStackTrace
           $ "Num.(*){Idx " ++ show (dim @d) ++ "}: cannot negate index."
    {-# INLINE negate #-}
    abs = id
    {-# INLINE abs #-}
    signum _ = Idx 1
    {-# INLINE signum #-}

#ifdef UNSAFE_INDICES
    fromInteger = unsafeCoerce (fromInteger :: Integer -> Int)
#else
    fromInteger i
      | i >= 0 && i < d = Idx $ fromInteger i
      | otherwise       = errorWithoutStackTrace
                        $ "Num.fromInteger{Idx "
                        ++ show d ++ "}: integer "
                        ++ show i ++ " is outside of index bounds."
      where
        d = toInteger (unsafeCoerce (dim @d) :: Int)
#endif
    {-# INLINE fromInteger #-}
-}
