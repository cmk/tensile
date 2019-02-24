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
 --
import Data.Function (on)
import Numeric.Tensile.Dimensions.Dims
import Numeric.Tensile.Dimensions.Idx.Types
import Numeric.Tensile.Dimensions.Types hiding (take)

import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Finite as F

import GHC.TypeLits (KnownNat(..))
import           Control.Arrow           (first)
import           Foreign.Storable        (Storable)
import           GHC.Base
import           GHC.Enum


-- | Type-level dimensional indexing with arbitrary Int values inside.
--   Most of the operations on it require `KnownDims` constraint,
--   because the @Idxs@ itself does not store info about dimension bounds.
type Idxs (ds :: [Nat]) = TypedList Idx ds

listIdxs :: Num n => Idxs ds -> [n]
listIdxs ds = listVals ds idxVal
{-# INLINE listIdxs #-}


{-
    toEnum i = go dsd $ fromIntegral i
      where
        dsd = dims @ds
        go :: forall ns . Dims ns -> Int -> Idxs ns
        go S 0 = S
        go S _ = error ("Idxs ") -- ++ show (listDims dsd)) TODO fix
        go (Snoc ds d) off = case divMod off (dimVal d) of
          (off', j) -> go ds off' `snoc` Idx j
-}

{-
idxsFromInts :: forall ds . KnownDims ds => [Int] -> Idxs ds
idxsFromInts = go (dims @ds)
  where
    go S [] = S
    go (d :+ ds) (i : is) = (reifyDim d (idx i) :+) <$> go ds is   -- | i >= 0 && i < d = (i:) <$> go ds is



_Idxs = KnownDims d => Iso' Int (Idxs ds)
_Idxs = iso idxs fromIdxs
-}



majorToMinor :: forall ds i. Integral i => Dims ds -> Idxs ds -> i
majorToMinor dims = fromIntegral . go 1 dims
  where
    go :: forall ns . Int -> Dims ns -> Idxs ns -> Int
    go _ S S                     = 0
    go m (d :+ ds) (Idx i :+ is) = m * i + go (m * dimVal d) ds is

-- TODO use withPerm instead
minorToMajor :: forall ds i. Integral i => Dims ds -> Idxs ds -> i
minorToMajor d i = majorToMinor (unsafeReverse d) (unsafeReverse i)

fromIdxs :: forall ds i. Integral i => Dims ds -> Idxs ds -> i
fromIdxs = minorToMajor

idxs :: forall d i. Integral i => Dims d -> i -> Idxs d
idxs d = go d . flip mod (size d) . fromIntegral
  where
    go :: forall d . Dims d -> Int -> Idxs d
    go S _ = S
    --go S _ = S error ("Idxs ") -- ++ show (listDims dsd)) TODO: fix
    go (Snoc ds d) off = case divMod off (dimVal d) of
      (off', j) -> go ds off' `snoc` Idx j

--------------------------------------------------------------------------------

liftIdxs :: Dims d -> (Int -> Int) -> Idxs d -> Idxs d
liftIdxs d f = idxs d . flip mod (size d) . f . minorToMajor d

liftIdxs2 :: Dims d -> (Int -> Int -> Int) -> Idxs d -> Idxs d -> Idxs d
liftIdxs2 d f = on k $ minorToMajor d
  where k i j = idxs d . flip mod (size d) $ f i j

-- diffIdxs =? fromEnum $ reflectDims $ liftIdxs2 (-)
diffIdxs :: Dims ds -> Idxs ds -> Idxs ds -> Int
diffIdxs d i j = _diffIdxs (unsafeReverse d) (unsafeReverse i) (unsafeReverse j)
{-# INLINE diffIdxs #-}

-- | Offset difference of two indices @idx1 - idx2@
_diffIdxs :: Dims ds -> Idxs ds -> Idxs ds -> Int
_diffIdxs S S S = 0
_diffIdxs (d :+ ds) (Idx i1 :+ is1) (Idx i2 :+ is2)
  = fromIntegral i1 - fromIntegral i2
  + fromIntegral (dimVal d) * _diffIdxs ds is1 is2

--TODO this funtion seems broken
-- | Step dimension index by an Int offset
stepIdxs :: Dims ds -> Int -> Idxs ds -> Idxs ds
stepIdxs d di i = _stepIdxs (unsafeReverse d) di (unsafeReverse i)
{-# INLINE stepIdxs #-}

_stepIdxs :: Dims ds -> Int -> Idxs ds -> Idxs ds
_stepIdxs S _ S = S
_stepIdxs (d :+ ds) di (Idx i :+ is)
      = case divMod (di + fromIntegral i) (fromIntegral (dimVal d)) of
         (0  , i') -> Idx (fromIntegral i') :+ is
         (di', i') -> Idx (fromIntegral i') :+ _stepIdxs ds di' is


--------------------------------------------------------------------------------


instance Eq (Idxs ds) where
    a == b = (listIdxs a) == (listIdxs b)
    {-# INLINE (==) #-}


-- | Compare indices by their importance in lexicorgaphic order
--   from the last dimension to the first dimension
--   (the last dimension is the most significant one) @O(Length ds)@.
--
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

maxBound' :: forall ds . Dims ds -> Idxs ds
maxBound' S         = S
maxBound' (d :+ ds) = Idx (dimVal d - 1) :+ maxBound' ds

minBound' :: forall ds . Dims ds -> Idxs ds
minBound' S         = S
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
        go S S = succError $ "Idxs " -- ++ show (listDims $ dims @ds) TODO fix
        go (d :+ ds) (Idx i :+ is)
          | i == dimVal d = Idx 0 :+ go ds is
          | otherwise     = Idx (i+1) :+ is
    {-# INLINE succ #-}

    pred = go (dims @ds)
      where
        go :: forall ns . Dims ns -> Idxs ns -> Idxs ns
        go S S = predError $ "Idxs " -- ++ show (listDims $ dims @ds) TODO fix
        go (d :+ ds) (Idx i :+ is)
          | i == 0    = Idx (dimVal d) :+ go ds is
          | otherwise = Idx (i-1) :+ is
    {-# INLINE pred #-}

    toEnum = idxs (dims @ds)
    {-# INLINE toEnum #-}

    fromEnum = minorToMajor (dims @ds)
    {-# INLINE fromEnum #-}
{-
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
-}

instance KnownDims d => Semigroup (Idxs d) where
    (<>) = reflectDims @d liftIdxs2 (+)
    {-# INLINE (<>) #-}

instance KnownDims d => Monoid (Idxs d) where
    mempty = reflectDims @d idxs 0
    {-# INLINE mempty #-}

instance KnownDims d => Num (Idxs d) where

    (+) = reflectDims @d liftIdxs2 (+)

    (-) = reflectDims @d liftIdxs2 (-)
    {-# INLINE (-) #-}

    (*) = reflectDims @d liftIdxs2 (*)
    {-# INLINE (*) #-}

    signum _ = fromInteger 1
    {-# INLINE signum #-}

    abs = reflectDims @d liftIdxs abs
    {-# INLINE abs #-}

    fromInteger = reflectDims @d idxs . fromIntegral
    {-# INLINE fromInteger #-}
