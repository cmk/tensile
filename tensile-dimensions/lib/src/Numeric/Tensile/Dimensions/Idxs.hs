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

module Numeric.Tensile.Dimensions.Idxs (
  module Numeric.Tensile.Dimensions.Idxs,
  module Numeric.Tensile.Dimensions.Idx.Types,
  module Numeric.Tensile.Dimensions.Idxs.Types,
) where


import Control.Arrow (first)
import Control.Monad ((>=>))
import GHC.Base
import GHC.Enum
import GHC.TypeLits (KnownNat(..))
import Unsafe.Coerce (unsafeCoerce)

import Numeric.Tensile.Dimensions.Dims
import Numeric.Tensile.Dimensions.Idx.Types
import Numeric.Tensile.Dimensions.Idxs.Types
import Numeric.Tensile.Dimensions.Perm
import Numeric.Tensile.Dimensions.Types

import qualified Data.Finite as F
import qualified Math.Combinat.Permutations as P


-- | Remaps the index argument to the index with the same 'Int' representation 
-- under the permuted dimensions.
remapIdxs 
  :: forall ds r
   . Perm (Rank ds) 
  -> Dims ds 
  -> Idxs ds 
  -> (forall (ds' :: [Nat]). Dims ds' -> Idxs ds' -> r) 
  -> r
remapIdxs (Perm p) ds ix f = 
  unsafeReifyDims (P.permuteList p $ fromDims ds) $ \ds' -> 
    f ds' (toIdxs ds' . fromIdxs ds $ ix)

transposeIdxs 
  :: forall ds r
  . (forall ds'. Dims ds' -> Idxs ds' -> r)
  -> Dims ds -> Idxs ds -> r
transposeIdxs f ds ix = 
  unsafeReifyDims (Prelude.reverse $ fromDims ds) $ \ds' -> 
    f ds' (toIdxs ds' . fromIdxs ds $ ix)

{-
  unsafeReifyDims' (P.permuteList p $ fromDims ds) $ \ds' -> 
    f (reflect ds') (toIdxs (reflect ds') . fromIdxs ds $ ix)

remapIdxs' 
  :: forall (ds :: [Nat]) (ds' :: [Nat]) r. Permutable ds ds'
  => Perm (Rank ds) 
  -> Dims ds 
  -> Idxs ds 
  -> (KnownDims ds' => r) 
  -> r
remapIdxs' p ds ix f = 
  reifySomeDims (unsafePermute p ds) f

-- | Transform a permutation of tensor modes into a permutation of array indices.
-- transpose (lowerPerm p1) . transpose (lowerPerm p2) == transpose (lowerPerm $ p1 <> p2)
lowerPerm 
  :: forall ds. KnownDim (Size ds) 
  => Dims ds 
  -> Perm (Rank ds) -- ^ Rank-level permutation
  -> (Dims ds -> Idxs ds -> Perm (Rank ds) -> Perm (Size ds)) -- ^ Index filter
  -> Perm (Size ds)  -- ^ Index-level permutation
lowerPerm d p f = D.foldDimIdx d (\i p' -> p' <> f d i p) (mempty :: Perm (Size ds))

> :t forM_
forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
> :t foldM
foldM
  :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b

-}


-------------------------------------------------------------------------------
-- Indexed folds.
-------------------------------------------------------------------------------

-- | Fold over all dimensions keeping track of index
foldIdxs :: Dims ds -> (Idxs ds -> a -> a) -> a -> a
foldIdxs S k = k S
foldIdxs (Snoc ds d) k = foldIdxs ds k'
  where k' is = go 0
          where go i | i >= fromDim d = id
                     | otherwise = go (i + 1) . k (is `snoc` Idx i)
{-# INLINE foldIdxs #-}

foldMIdxs :: Monad m => Dims ds -> (Idxs ds -> a -> m a) -> a -> m a
foldMIdxs S k = k S
foldMIdxs (Snoc ds d) k = foldMIdxs ds k'
  where k' is = go 0
          where go i | i >= fromDim d = return
                     | otherwise = k (is `snoc` Idx i) >=> go (i + 1)
{-# INLINE foldMIdxs #-}

forMIdxs_ :: Monad m => Dims ds -> (Idxs ds -> m ()) -> m ()
forMIdxs_ S k = k S
forMIdxs_ (Snoc ds d) k = forMIdxs_ ds k'
  where k' is = go 0
          where go i | i >= fromDim d = return ()
                     | otherwise = k (is `snoc` Idx i) >> go (i + 1)
{-# INLINE forMIdxs_ #-}


foldDimsPartIdx :: Idxs ds -> Idxs ds -> (Idxs ds -> a -> a) -> a -> a
foldDimsPartIdx s e k = _foldDimsPartIdx (unsafeReverse s) (unsafeReverse e) k

-- TODO reimplement bounds ord check 
_foldDimsPartIdx :: Idxs ds -> Idxs ds -> (Idxs ds -> a -> a) -> a -> a
_foldDimsPartIdx S S k = k S
_foldDimsPartIdx (start :+ starts) (end :+ ends) k
  | iEnd >= iStart = _foldDimsPartIdx starts ends (loop iStart)
  | otherwise      = _foldDimsPartIdx starts ends (looi iStart)
  where
    Idx iStart = start
    Idx iEnd   = end
    loop i is
      | i > iEnd = id
      | otherwise = k (Idx i :+ is) . loop (i+1) is
    looi i is
      | i < iEnd = id
      | otherwise = k (Idx i :+ is) . looi (i-1) is

{-
-- TODO convert to row major
overDimPartIdx_ :: Monad m
               => Idxs ds -- ^ Initial indices
               -> Idxs ds -- ^ Final indices
               -> (Idxs ds -> m ())
                          -- ^ Function to call on each dimension
               -> m ()
overDimPartIdx_ S S k = k S
overDimPartIdx_ (start :+ starts) (end :+ ends) k
  | iEnd >= iStart = overDimPartIdx_ starts ends loop'
  | otherwise      = overDimPartIdx_ starts ends looi'
  where
    Idx iStart = start
    Idx iEnd   = end
    loop' is = loop iStart
      where
        loop i
          | i > iEnd = return ()
          | otherwise = k (Idx i :+ is) >> loop (i+1)
    looi' is = looi iStart
      where
        looi i
          | i < iEnd = return ()
          | otherwise = k (Idx i :+ is) >> looi (i-1)

-- TODO probably just delete this
-- | Go over all dimensions keeping track of index and offset
overDim_ :: Monad m
         => Dims ds                  -- ^ Tensor dimensions
         -> (Idxs ds -> Int -> m ()) -- ^ Function to call on each dimension
         -> Int                      -- ^ Initial offset
         -> Int                      -- ^ Offset step
         -> m ()
overDim_ U k offset _step = k S offset
overDim_ (Snoc ds d) k offset step = overDim_ ds k' offset step -- (di * step)
  where
    dw = fromDim d
    -- di = fromIntegral dw
    k' is = go 0
      where
        go i off
          | i >= dw = return ()
          | otherwise = k (is `snoc` Idx i) off >> go (i+1) (off+step)

-}





