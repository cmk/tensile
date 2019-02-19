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

module Numeric.Tensile.Dimensions.Idxs.Types where

--import Numeric.KnownDims.Idxs (Idx(..), Idxs(..))
 --(Nat, TypedList(..), Dims(..), Dim(..), KnownDim(..), KnownDims(..), Permutable, Size, Rank)
import Numeric.Tensile.Dimensions.Dims
import Numeric.Tensile.Dimensions.Idx.Types
import Numeric.Tensile.Dimensions.Types


import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Finite as F

import GHC.TypeLits (KnownNat(..))
import           Control.Arrow           (first)
import           Foreign.Storable        (Storable)
import           GHC.Base
import           GHC.Enum


-- | Type-level dimensional indexing with arbitrary Word values inside.
--   Most of the operations on it require `KnownDims` constraint,
--   because the @Idxs@ itself does not store info about dimension bounds.
type Idxs (ds :: [Nat]) = TypedList Idx ds

listIdxs :: Idxs ds -> [Word]
listIdxs = unsafeCoerce
{-# INLINE listIdxs #-}

idxsFromWords :: forall ds . KnownDims ds => [Word] -> Maybe (Idxs ds)
idxsFromWords = unsafeCoerce . go (fromDims (dims @ds))
  where
    go [] [] = Just []
    go (d : ds) (i : is) | i >= 0 && i < d = (i:) <$> go ds is
    go _ _   = Nothing

majorToMinor :: forall ds i. Integral i => Dims ds -> Idxs ds -> i
majorToMinor dims = fromIntegral . go 1 dims
  where
    go :: forall ns . Word -> Dims ns -> Idxs ns -> Word
    go _ U U                     = 0
    go m (d :+ ds) (Idx i :+ is) = m * i + go (m * fromDim d) ds is

minorToMajor :: forall ds i. Integral i => Dims ds -> Idxs ds -> i
minorToMajor d i = majorToMinor (unsafeReverse d) (unsafeReverse i)

fromIdxs :: forall ds i. Integral i => Dims ds -> Idxs ds -> i
fromIdxs = minorToMajor

toIdxs :: forall ds i. Integral i => Dims ds -> i -> Idxs ds
toIdxs dsd i = go dsd $ fromIntegral i
  where
    go :: forall ns . Dims ns -> Word -> Idxs ns
    go U 0 = U
    go U _ = error ("Idxs ") -- ++ show (fromDims dsd)) TODO: fix
    go (Snoc ds d) off = case divMod off (fromDim d) of
      (off', j) -> go ds off' `snoc` Idx j

instance Eq (Idxs ds) where
    (==) = unsafeCoerce# ((==) :: [Word] -> [Word] -> Bool)
    {-# INLINE (==) #-}

-- TODO check if ok to not reverse this
--
-- | Compare indices by their importance in lexicorgaphic order
--   from the last dimension to the first dimension
--   (the last dimension is the most significant one) @O(Length ds)@.
--
--   Literally,
--
--   > compare a b = compare (reverse $ listIdxs a) (reverse $ listIdxs b)
--
--   This is the same @compare@ rule, as for `Dims`.
--   Another reason to reverse the list of indices is to have a consistent
--   behavior when calculating index offsets:
--
--   > sort == sortOn fromEnum
--
instance Ord (Idxs ds) where
    compare a b = compare (listIdxs a) (listIdxs b)
    {-# INLINE compare #-}


instance Show (Idxs ds) where
    show ds = "Idxs " ++ show (listIdxs ds)
    showsPrec p ds
      = showParen (p >= 10)
      $ showString "Idxs " . showsPrec p (listIdxs ds)

{-
-- | With this instance we can slightly reduce indexing expressions, e.g.
--
--   > x ! (1 :+ 2 :+ 4) == x ! (1 :+ 2 :+ 4 :+ U)
--
instance KnownDim n => Num (Idxs '[n]) where
    (a:+U) + (b:+U) = (a+b) :+ U
    {-# INLINE (+) #-}
    (a:+U) - (b:+U) = (a-b) :+ U
    {-# INLINE (-) #-}
    (a:+U) * (b:+U) = (a*b) :+ U
    {-# INLINE (*) #-}
    signum (a:+U)   = signum a :+ U
    {-# INLINE signum #-}
    abs (a:+U)      = abs a :+ U
    {-# INLINE abs #-}
    fromInteger i   = fromInteger i :+ U
    {-# INLINE fromInteger #-}
-}

maxBound' :: forall ds . Dims ds -> Idxs ds
maxBound' U         = U
maxBound' (d :+ ds) = Idx (fromDim d - 1) :+ maxBound' ds

minBound' :: forall ds . Dims ds -> Idxs ds
minBound' U         = U
minBound' (_ :+ ds) = Idx 0 :+ minBound' ds

instance KnownDims ds => Bounded (Idxs ds) where
    maxBound = maxBound' (dims @ds)
    {-# INLINE maxBound #-}
    minBound = minBound' (dims @ds)
    {-# INLINE minBound #-}

instance KnownDims ds => Enum (Idxs ds) where

    succ = go (dims @ds)
      where
        go :: forall ns . Dims ns -> Idxs ns -> Idxs ns
        go U U = succError $ "Idxs " -- ++ show (fromDims $ dims @ds) TODO fix
        go (d :+ ds) (Idx i :+ is)
          | i == fromDim d = Idx 0 :+ go ds is
          | otherwise     = Idx (i+1) :+ is
    {-# INLINE succ #-}

    pred = go (dims @ds)
      where
        go :: forall ns . Dims ns -> Idxs ns -> Idxs ns
        go U U = predError $ "Idxs " -- ++ show (fromDims $ dims @ds) TODO fix
        go (d :+ ds) (Idx i :+ is)
          | i == 0    = Idx (fromDim d) :+ go ds is
          | otherwise = Idx (i-1) :+ is
    {-# INLINE pred #-}

    toEnum i = go dsd $ fromIntegral i
      where
        dsd = dims @ds
        go :: forall ns . Dims ns -> Word -> Idxs ns
        go U 0 = U
        go U _ = error ("Idxs ") -- ++ show (fromDims dsd)) TODO fix
        go (Snoc ds d) off = case divMod off (fromDim d) of
          (off', j) -> go ds off' `snoc` Idx j
    {-# INLINE toEnum #-}


    fromEnum = minorToMajor (dims @ds)
    {-# INLINE fromEnum #-}

    enumFrom x = take (diffIdxs (dims @ds) maxBound x + 1) $ iterate succ x
    {-# INLINE enumFrom #-}

    enumFromTo x y | x >= y    = take (diffIdxs ds x y + 1) $ iterate pred x
                   | otherwise = take (diffIdxs ds y x + 1) $ iterate succ x
      where
        ds = dims @ds
    {-# INLINE enumFromTo #-}

    enumFromThen x x' = take n $ iterate (stepIdxs ds dn) x
      where
        ds = dims @ds
        dn = diffIdxs ds x' x
        n  = 1 + if dn == 0
                 then 0
                 else if dn > 0
                      then diffIdxs ds maxBound x `div` dn
                      else diffIdxs ds x minBound `div` negate dn
    {-# INLINE enumFromThen #-}

    enumFromThenTo x x' y = take n $ iterate (stepIdxs ds dn) x
      where
        ds = dims @ds
        dn = diffIdxs ds x' x
        n  = 1 + if dn == 0 then 0
                            else diffIdxs ds y x `div` dn
    {-# INLINE enumFromThenTo #-}



--------------------------------------------------------------------------------



diffIdxs :: Dims ds -> Idxs ds -> Idxs ds -> Int
diffIdxs d i j = _diffIdxs (unsafeReverse d) (unsafeReverse i) (unsafeReverse j)
{-# INLINE diffIdxs #-}

-- | Offset difference of two indices @idx1 - idx2@
_diffIdxs :: Dims ds -> Idxs ds -> Idxs ds -> Int
_diffIdxs U U U = 0
_diffIdxs (d :+ ds) (Idx i1 :+ is1) (Idx i2 :+ is2)
  = fromIntegral i1 - fromIntegral i2
  + fromIntegral (fromDim d) * _diffIdxs ds is1 is2

-- | Step dimension index by an Int offset
stepIdxs :: Dims ds -> Int -> Idxs ds -> Idxs ds
stepIdxs d di i = _stepIdxs (unsafeReverse d) di (unsafeReverse i)
{-# INLINE stepIdxs #-}

_stepIdxs :: Dims ds -> Int -> Idxs ds -> Idxs ds
_stepIdxs U _ U = U
_stepIdxs (d :+ ds) di (Idx i :+ is)
      = case divMod (di + fromIntegral i) (fromIntegral (fromDim d)) of
         (0  , i') -> Idx (fromIntegral i') :+ is
         (di', i') -> Idx (fromIntegral i') :+ _stepIdxs ds di' is
