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


import GHC.TypeLits (KnownNat(..))
import Control.Arrow           (first)
import GHC.Base
import GHC.Enum

import Numeric.Tensile.Dimensions.Dim.Types
import Numeric.Tensile.Dimensions.Types as T


newtype Idx (d :: Nat) = Idx { unIdx :: Word } deriving (Eq, Ord)

unsafeIdxFromWord :: forall d. KnownDim d => Word -> Idx d
#ifdef UNSAFE_INDICES
unsafeIdxFromWord = unsafeCoerce#
#else
unsafeIdxFromWord w
  | w >= 0 && w < d = Idx w
  | otherwise       = errorWithoutStackTrace
                    $ "idxFromWord{Idx "
                    ++ show d ++ "}: word "
                    ++ show w ++ " is outside of index bounds."
  where
    d = unsafeCoerce# (dim @d)
#endif
{-# INLINE unsafeIdxFromWord #-}

idxFromWord :: forall d . KnownDim d => Word -> Maybe (Idx d)
idxFromWord w
  | w >= 0 && w < unsafeCoerce# (dim @d) = Just (Idx w)
  | otherwise                               = Nothing
{-# INLINE idxFromWord #-}

idxToWord :: Idx d -> Word
idxToWord = unsafeCoerce#
{-# INLINE idxToWord #-}

{-# RULES
"fromIntegral/idxToWord"
  fromIntegral = idxToWord
  #-}

idxFromFinite :: forall (d :: Nat). KnownDim d => F.Finite d -> Idx d
idxFromFinite = unsafeIdxFromWord . fromIntegral . F.getFinite

finiteFromIdx :: forall (d :: Nat). KnownNat d => Idx d -> F.Finite d
finiteFromIdx = F.finite . fromIntegral . idxToWord 

instance Read (Idx d) where
    readsPrec d = fmap (first Idx) . readsPrec d

instance Show (Idx d) where
    showsPrec d = showsPrec d . unIdx

instance KnownDim d => Bounded (Idx d) where
    minBound = Idx 0
    {-# INLINE minBound #-}
    maxBound = Idx $ max 0 $ unsafeCoerce (dim @d) - 1
    {-# INLINE maxBound #-}

instance KnownDim d => Enum (Idx d) where

#ifdef UNSAFE_INDICES
    succ = unsafeCoerce ((+ 1) :: Word -> Word)
#else
    succ x@(Idx i)
      | x /= maxBound = Idx (i + 1)
      | otherwise = succError $ "Idx " ++ show (dim @d)
#endif
    {-# INLINE succ #-}

#ifdef UNSAFE_INDICES
    pred = unsafeCoerce# ((+ (-1)) :: Word -> Word)
#else
    pred x@(Idx i)
      | x /= minBound = Idx (i - 1)
      | otherwise = predError $ "Idx " ++ show (dim @d)
#endif
    {-# INLINE pred #-}

#ifdef UNSAFE_INDICES
    toEnum (I# i#) = unsafeCoerce# (W# (int2Word# i#))
#else
    toEnum i@(I# i#)
        | i >= 0 && i < d' = unsafeCoerce# (W# (int2Word# i# ))
        | otherwise        = toEnumError ("Idx " ++ show d) i (0, d)
      where
        d = unsafeCoerce# (dim @d) :: Word
        d' = fromIntegral d
#endif
    {-# INLINE toEnum #-}

#ifdef UNSAFE_INDICES
    fromEnum (Idx (W# w#)) = I# (word2Int# w#)
#else
    fromEnum (Idx x@(W# w#))
        | x <= maxIntWord = I# (word2Int# w#)
        | otherwise       = fromEnumError ("Idx " ++ show (dim @d)) x
        where
          maxIntWord = W# (case maxInt of I# i -> int2Word# i)
#endif
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
      = unsafeCoerce# (enumFromTo :: Word -> Word -> [Word])
    {-# INLINE enumFromTo #-}
    enumFromThenTo
      = unsafeCoerce# (enumFromThenTo :: Word -> Word -> Word -> [Word])
    {-# INLINE enumFromThenTo #-}
-}

instance KnownDim d => Num (Idx d) where

#ifdef UNSAFE_INDICES
    (+) = unsafeCoerce ((+) :: Word -> Word -> Word)
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
    (-) = unsafeCoerce ((-) :: Word -> Word -> Word)
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
    (*) = unsafeCoerce ((*) :: Word -> Word -> Word)
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
    fromInteger = unsafeCoerce (fromInteger :: Integer -> Word)
#else
    fromInteger i
      | i >= 0 && i < d = Idx $ fromInteger i
      | otherwise       = errorWithoutStackTrace
                        $ "Num.fromInteger{Idx "
                        ++ show d ++ "}: integer "
                        ++ show i ++ " is outside of index bounds."
      where
        d = toInteger (unsafeCoerce (dim @d) :: Word)
#endif
    {-# INLINE fromInteger #-}

