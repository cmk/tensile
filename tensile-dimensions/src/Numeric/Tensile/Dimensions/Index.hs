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

module Numeric.Tensile.Dimensions.Index where

--import Numeric.KnownDims.Idxs (Idx(..), Idxs(..))
 --(Nat, TypedList(..), Dims(..), Dim(..), KnownDim(..), KnownDims(..), Permutable, Size, Rank)
import Numeric.Tensile.Dimensions.Dim
import Numeric.Tensile.Dimensions.Dims

import Numeric.Tensile.Dimensions.Permutation
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Finite as F
import qualified Math.Combinat.Permutations as P

import  Numeric.Tensile.Dimensions.Types as T

import GHC.TypeLits (KnownNat(..))
import           Control.Arrow           (first)
import           Foreign.Storable        (Storable)
import           GHC.Base
import           GHC.Enum

listDims = undefined

newtype Idx (d :: Nat) = Idx { unIdx :: Word }
  deriving ( Storable, Eq, Ord )

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

{-
instance KnownDim n => Num (Idx n) where

#ifdef UNSAFE_INDICES
    (+) = unsafeCoerce# ((+) :: Word -> Word -> Word)
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
        d = unsafeCoerce# (dim @d)
#endif
    {-# INLINE (+) #-}

#ifdef UNSAFE_INDICES
    (-) = unsafeCoerce# ((-) :: Word -> Word -> Word)
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
    (*) = unsafeCoerce# ((*) :: Word -> Word -> Word)
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
        d = unsafeCoerce# (dim @d)
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
    fromInteger = unsafeCoerce# (fromInteger :: Integer -> Word)
#else
    fromInteger i
      | i >= 0 && i < d = Idx $ fromInteger i
      | otherwise       = errorWithoutStackTrace
                        $ "Num.fromInteger{Idx "
                        ++ show d ++ "}: integer "
                        ++ show i ++ " is outside of index bounds."
      where
        d = toInteger (unsafeCoerce# (dim @d) :: Word)
#endif
    {-# INLINE fromInteger #-}
-}

-- | Type-level dimensional indexing with arbitrary Word values inside.
--   Most of the operations on it require `KnownDims` constraint,
--   because the @Idxs@ itself does not store info about dimension bounds.
--
--   Note, this type has a special `Enum` instance:
--   `fromEnum` gives an offset of the index in a flat 1D array;
--   this is in line with a weird `Enum` instance of `Idx` type.
type Idxs (xs :: [Nat]) = TypedList Idx xs


listIdxs :: Idxs ds -> [Word]
listIdxs = unsafeCoerce#
{-# INLINE listIdxs #-}



idxsFromWords :: forall ds . KnownDims ds => [Word] -> Maybe (Idxs ds)
idxsFromWords = unsafeCoerce . go (listDims (dims @ds))
  where
    go [] [] = Just []
    go (d : ds) (i : is) | i >= 0 && i < d = (i:) <$> go ds is
    go _ _   = Nothing

majorToMinor :: forall ds i. Integral i => Dims ds -> Idxs ds -> i
majorToMinor dims = fromIntegral . go 1 dims
  where
    go :: forall ns . Word -> Dims ns -> Idxs ns -> Word
    go _ U U                     = 0
    go m (d :* ds) (Idx i :* is) = m * i + go (m * dimVal' d) ds is

minorToMajor :: forall ds i. Integral i => Dims ds -> Idxs ds -> i
minorToMajor d i = majorToMinor (_reversed d) (_reversed i)

fromIdxs :: forall ds i. Integral i => Dims ds -> Idxs ds -> i
fromIdxs = minorToMajor

toIdxs :: forall ds i. Integral i => Dims ds -> i -> Idxs ds
toIdxs dsd i = go dsd $ fromIntegral i
  where
    go :: forall ns . Dims ns -> Word -> Idxs ns
    go U 0 = U
    go U _ = error ("Idxs ") -- ++ show (listDims dsd)) TODO: fix
    go (T.Snoc ds d) off = case divMod off (dimVal' d) of
      (off', j) -> go ds off' `T.snoc` Idx j

instance Eq (Idxs xs) where
    (==) = unsafeCoerce# ((==) :: [Word] -> [Word] -> Bool)
    {-# INLINE (==) #-}

-- TODO check if ok to not reverse this
--
-- | Compare indices by their importance in lexicorgaphic order
--   from the last dimension to the first dimension
--   (the last dimension is the most significant one) @O(Length xs)@.
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
--   > x ! (1 :* 2 :* 4) == x ! (1 :* 2 :* 4 :* U)
--
instance KnownDim n => Num (Idxs '[n]) where
    (a:*U) + (b:*U) = (a+b) :* U
    {-# INLINE (+) #-}
    (a:*U) - (b:*U) = (a-b) :* U
    {-# INLINE (-) #-}
    (a:*U) * (b:*U) = (a*b) :* U
    {-# INLINE (*) #-}
    signum (a:*U)   = signum a :* U
    {-# INLINE signum #-}
    abs (a:*U)      = abs a :* U
    {-# INLINE abs #-}
    fromInteger i   = fromInteger i :* U
    {-# INLINE fromInteger #-}
-}

maxBound' :: forall ds . Dims ds -> Idxs ds
maxBound' U         = U
maxBound' (d :* ds) = Idx (dimVal' d - 1) :* maxBound' ds

minBound' :: forall ds . Dims ds -> Idxs ds
minBound' U         = U
minBound' (_ :* ds) = Idx 0 :* minBound' ds

instance KnownDims ds => Bounded (Idxs ds) where
    maxBound = maxBound' (dims @ds)
    {-# INLINE maxBound #-}
    minBound = minBound' (dims @ds)
    {-# INLINE minBound #-}

instance KnownDims ds => Enum (Idxs ds) where

    succ = go (dims @ds)
      where
        go :: forall ns . Dims ns -> Idxs ns -> Idxs ns
        go U U = succError $ "Idxs " -- ++ show (listDims $ dims @ds) TODO fix
        go (d :* ds) (Idx i :* is)
          | i == dimVal' d = Idx 0 :* go ds is
          | otherwise     = Idx (i+1) :* is
    {-# INLINE succ #-}

    pred = go (dims @ds)
      where
        go :: forall ns . Dims ns -> Idxs ns -> Idxs ns
        go U U = predError $ "Idxs " -- ++ show (listDims $ dims @ds) TODO fix
        go (d :* ds) (Idx i :* is)
          | i == 0    = Idx (dimVal' d) :* go ds is
          | otherwise = Idx (i-1) :* is
    {-# INLINE pred #-}

    toEnum i = go dsd $ fromIntegral i
      where
        dsd = dims @ds
        go :: forall ns . Dims ns -> Word -> Idxs ns
        go U 0 = U
        go U _ = error ("Idxs ") -- ++ show (listDims dsd)) TODO fix
        go (T.Snoc ds d) off = case divMod off (dimVal' d) of
          (off', j) -> go ds off' `T.snoc` Idx j
    {-# INLINE toEnum #-}


    fromEnum = minorToMajor (dims @ds)
    {-# INLINE fromEnum #-}

    enumFrom x = take (diffIdx (dims @ds) maxBound x + 1) $ iterate succ x
    {-# INLINE enumFrom #-}

    enumFromTo x y | x >= y    = take (diffIdx ds x y + 1) $ iterate pred x
                   | otherwise = take (diffIdx ds y x + 1) $ iterate succ x
      where
        ds = dims @ds
    {-# INLINE enumFromTo #-}

    enumFromThen x x' = take n $ iterate (stepIdx ds dn) x
      where
        ds = dims @ds
        dn = diffIdx ds x' x
        n  = 1 + if dn == 0
                 then 0
                 else if dn > 0
                      then diffIdx ds maxBound x `div` dn
                      else diffIdx ds x minBound `div` negate dn
    {-# INLINE enumFromThen #-}

    enumFromThenTo x x' y = take n $ iterate (stepIdx ds dn) x
      where
        ds = dims @ds
        dn = diffIdx ds x' x
        n  = 1 + if dn == 0 then 0
                            else diffIdx ds y x `div` dn
    {-# INLINE enumFromThenTo #-}



--------------------------------------------------------------------------------



diffIdx :: Dims ds -> Idxs ds -> Idxs ds -> Int
diffIdx d i j = _diffIdx (_reversed d) (_reversed i) (_reversed j)
{-# INLINE diffIdx #-}

-- | Offset difference of two indices @idx1 - idx2@
_diffIdx :: Dims ds -> Idxs ds -> Idxs ds -> Int
_diffIdx U U U = 0
_diffIdx (d :* ds) (Idx i1 :* is1) (Idx i2 :* is2)
  = fromIntegral i1 - fromIntegral i2
  + fromIntegral (dimVal' d) * _diffIdx ds is1 is2

-- | Step dimension index by an Int offset
stepIdx :: Dims ds -> Int -> Idxs ds -> Idxs ds
stepIdx d di i = _stepIdx (_reversed d) di (_reversed i)
{-# INLINE stepIdx #-}

_stepIdx :: Dims ds -> Int -> Idxs ds -> Idxs ds
_stepIdx U _ U = U
_stepIdx (d :* ds) di (Idx i :* is)
      = case divMod (di + fromIntegral i) (fromIntegral (dimVal' d)) of
         (0  , i') -> Idx (fromIntegral i') :* is
         (di', i') -> Idx (fromIntegral i') :* _stepIdx ds di' is







-- (Idx(),Idxs(),listIdxs,idxToWord,idxFromWord,unsafeIdxFromWord)

--_permuted :: Permutable d d' => Perm (Rank d) -> TypedList f d -> TypedList f d'
_permuted :: Perm (Rank d) -> TypedList f d -> TypedList f d'
_permuted (Perm p) = unsafeCoerce . P.permuteList p . unsafeCoerce

-- TODO unsafe, remove
_reversed :: TypedList f d -> TypedList f d'
_reversed = unsafeCoerce . Prelude.reverse . unsafeCoerce 


-- | Remaps the index argument to the index with the same 'Int' representation under the permuted dimensions.
remapIdxs 
  :: forall (ds :: [Nat]) r. Perm (Rank ds) 
  -> Dims ds 
  -> Idxs ds 
  -> (forall (ds' :: [Nat]). Dims ds' -> Idxs ds' -> r) 
  -> r
remapIdxs (Perm p) ds ix f = 
  unsafeReifyDims' (P.permuteList p $ listDims ds) $ \ds' -> 
    f (T.reflect ds') (toIdxs (T.reflect ds') . fromIdxs ds $ ix)


{-
remapIdxs' 
  :: forall (ds :: [Nat]) (ds' :: [Nat]) r. Permutable ds ds'
  => Perm (Rank ds) 
  -> Dims ds 
  -> Idxs ds 
  -> (KnownDims ds' => r) 
  -> r
remapIdxs' p ds ix f = 
  T.reifyDims (_permuted p ds) f

-- | Transform a permutation of tensor modes into a permutation of array indices.
-- transpose (lowerPerm p1) . transpose (lowerPerm p2) == transpose (lowerPerm $ p1 <> p2)
lowerPerm 
  :: forall ds. KnownDim (Size ds) 
  => Dims ds 
  -> Perm (Rank ds) -- ^ Rank-level permutation
  -> (Dims ds -> Idxs ds -> Perm (Rank ds) -> Perm (Size ds)) -- ^ Index filter
  -> Perm (Size ds)  -- ^ Index-level permutation
lowerPerm d p f = D.foldDimIdx d (\i p' -> p' <> f d i p) (mempty :: Perm (Size ds))

-}



overDimIdx_ :: Monad m
            => Dims ds               -- ^ Tensor dimensions
            -> (Idxs ds -> m ())     -- ^ Function to call on each dimension
            -> m ()
overDimIdx_ U k = k U
overDimIdx_ (T.Snoc ds d) k = overDimIdx_ ds k'
  where
    dw = dimVal' d
    k' is = go 0
      where
        go i
          | i >= dw = return ()
          | otherwise = k (is `T.snoc` Idx i) >> go (i+1)

-- TODO reimplement bounds ord check 
_foldDimPartIdx :: Idxs ds -- ^ Initial indices
               -> Idxs ds -- ^ Final indices
               -> (Idxs ds -> a -> a)
                          -- ^ Function to call on each dimension
               -> a       -- ^ initial value
               -> a
_foldDimPartIdx U U k = k U
_foldDimPartIdx (start :* starts) (end :* ends) k
  | iEnd >= iStart = _foldDimPartIdx starts ends (loop iStart)
  | otherwise      = _foldDimPartIdx starts ends (looi iStart)
  where
    Idx iStart = start
    Idx iEnd   = end
    loop i is
      | i > iEnd = id
      | otherwise = k (Idx i :* is) . loop (i+1) is
    looi i is
      | i < iEnd = id
      | otherwise = k (Idx i :* is) . looi (i-1) is

foldDimPartIdx s e k = foldDimPartIdx (_reversed s) (_reversed e) k
{-
-- TODO convert to row major
overDimPartIdx_ :: Monad m
               => Idxs ds -- ^ Initial indices
               -> Idxs ds -- ^ Final indices
               -> (Idxs ds -> m ())
                          -- ^ Function to call on each dimension
               -> m ()
overDimPartIdx_ U U k = k U
overDimPartIdx_ (start :* starts) (end :* ends) k
  | iEnd >= iStart = overDimPartIdx_ starts ends loop'
  | otherwise      = overDimPartIdx_ starts ends looi'
  where
    Idx iStart = start
    Idx iEnd   = end
    loop' is = loop iStart
      where
        loop i
          | i > iEnd = return ()
          | otherwise = k (Idx i :* is) >> loop (i+1)
    looi' is = looi iStart
      where
        looi i
          | i < iEnd = return ()
          | otherwise = k (Idx i :* is) >> looi (i-1)

-- TODO probably just delete this
-- | Go over all dimensions keeping track of index and offset
overDim_ :: Monad m
         => Dims ds                  -- ^ Tensor dimensions
         -> (Idxs ds -> Int -> m ()) -- ^ Function to call on each dimension
         -> Int                      -- ^ Initial offset
         -> Int                      -- ^ Offset step
         -> m ()
overDim_ U k offset _step = k U offset
overDim_ (T.Snoc ds d) k offset step = overDim_ ds k' offset step -- (di * step)
  where
    dw = dimVal' d
    -- di = fromIntegral dw
    k' is = go 0
      where
        go i off
          | i >= dw = return ()
          | otherwise = k (is `T.snoc` Idx i) off >> go (i+1) (off+step)

-}





