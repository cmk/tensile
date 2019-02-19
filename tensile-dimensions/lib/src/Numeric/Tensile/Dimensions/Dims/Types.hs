{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE UndecidableInstances   #-} 
{-# LANGUAGE ViewPatterns           #-}

{-# OPTIONS_GHC -fno-warn-orphans      #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RoleAnnotations           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UndecidableSuperClasses   #-}
{-# LANGUAGE ViewPatterns              #-}
module Numeric.Tensile.Dimensions.Dims.Types where

import Data.Functor.Identity
import Data.Proxy
import Unsafe.Coerce (unsafeCoerce)

import GHC.Base (Type)
import           GHC.Exts           (Constraint)
import GHC.TypeLits
import           Data.Type.Bool
import           Data.Type.Equality

import Numeric.Tensile.Dimensions.Dim.Types
import Numeric.Tensile.Dimensions.Types

-- | A list of 'Dim' used to express the shape of a tensor.
type Dims (ds :: [Nat]) = TypedList Dim ds

instance Eq (Dims ds) where
    (==) = unsafeCoerce ((==) :: [Word] -> [Word] -> Bool)
    {-# INLINE (==) #-}

instance Show (Dims ds) where
    show ds = "Dims " ++ show (fromDims ds)
    showsPrec p ds = showParen (p >= 10)
      $ showString "Dims " . showsPrec p (fromDims ds)

-- | Similar to `natVal` from `GHC.TypeLits`, but returns `Word`.
fromDims :: Dims ds -> [Word]
fromDims ds = elimDims ds fromDim --Numeric.Tensile.Dimensions.Types.map fromDim
{-# INLINE fromDims #-}

elimDims :: Dims ds -> (forall d. Dim d -> r) -> [r]
elimDims U _ = []
elimDims (d :* ds) f = f d : elimDims ds f

-- | Product of all dimension sizes.
size :: Dims ds -> Word
size = product . fromDims
{-# INLINE size #-}

compareDims :: Dims as -> Dims bs -> Ordering
compareDims as bs = compare (fromDims as) (fromDims bs)
{-# INLINE compareDims #-}

refineDims :: forall ds. KnownDims ds => (Dims ds -> Bool) -> Maybe (Dims ds)
refineDims p = reflectDims $ \x -> if p x then Just x else Nothing
{-# INLINE refineDims #-}

-- Starting from GHC 8.2, compiler supports specifying lists of complete
-- pattern synonyms.
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE Dims #-}
-- {-# COMPLETE Dims' #-}
#endif

-- | @O(1)@ Pattern-matching against this constructor brings a `Dimensions`
--   instance into the scope.
--   Thus, you can do arbitrary operations on your dims and use this pattern
--   at any time to reconstruct the class instance at runtime.
pattern Dims :: forall ds . KnownDims ds => Dims ds
pattern Dims <- (withDims -> E)
  where
    Dims = dims @ds

{-
-- | @O(Length ds)@ `KnownDims` and `KnownDim` for each individual dimension.
pattern Dims' :: forall ds . ()
                  => (All KnownDim ds, KnownDims ds) => Dims ds
pattern Dims' <- (patKDims -> PatKDims)
  where
    Dims' = dims @ds

data PatKDims ds = (All KnownDim ds, KnownDims ds) => PatKDims

patKDims :: Dims ns -> PatKDims ns
patKDims U = PatKDims
patKDims (Dim :* ns) = case patKDims ns of
  PatKDims -> PatKDims
#if __GLASGOW_HASKELL__ >= 802
#else
patKDims _ = impossible
#endif
{-# INLINE patKDims #-}
-}

-------------------------------------------------------------------------------
-- Existential 'Dims' type.
-------------------------------------------------------------------------------

type SomeDims = [SomeDim]

someDims :: [Word] -> Maybe SomeDims
someDims = traverse someDim
{-# INLINE someDims #-}

withSomeDims :: SomeDims -> (forall ds. KnownDims ds => Dims ds -> r) -> r 
withSomeDims []     f = f U
withSomeDims (x:xs) f = withSomeDim x $ \d ->
                          withSomeDims xs $ \ds -> f (d :* ds)
{-# INLINE withSomeDims #-}

-------------------------------------------------------------------------------
-- Value Reification
-------------------------------------------------------------------------------

reifyDims :: forall d r . Dims d -> (KnownDims d => r) -> r
reifyDims d k = unsafeCoerce (WithKnownDims k :: WithKnownDims d r) d

newtype WithKnownDims ds r = WithKnownDims (KnownDims ds => r)

unsafeReifyDims :: [Word] -> (forall ds. KnownDims ds => Dims ds -> r) -> r
unsafeReifyDims []     f = f U
unsafeReifyDims (x:xs) f = 
  unsafeReifyDim x $ \p ->
    unsafeReifyDims xs $ \ps -> f ((reflect p) :* (reflect ps))

-- @'reifyDims' d == withEvidence ('withDims' d)@ 
withDims :: Dims ds -> Evidence (KnownDims ds)
withDims ds = reifyDims ds E
{-# INLINE withDims #-}

-------------------------------------------------------------------------------
-- Type Reflection 
-------------------------------------------------------------------------------

-- | A convenience function useful when we need to name a dimensional value multiple times.
-- @d == 'reifyDims' d ('reflectDims' id)@ 
reflectDims :: forall ds r. KnownDims ds => (Dims ds -> r) -> r
reflectDims f = f Dims 

reflectDims2 
  :: forall as bs r. (KnownDims as, KnownDims bs) => (Dims as -> Dims bs -> r) -> r
reflectDims2 f = f Dims Dims 

-- | Put runtime evidence of `Dims` value inside function constraints.
--   Similar to `KnownDim` or `KnownNat`, but for lists of numbers.
class KnownDims ds where
    -- | Get dimensionality of a space at runtime,
    --   represented as a list of `Dim`.
    --
    --   Note, this function is supposed to be used with @TypeApplications@.
    --   For example, you can type:
    --
    --   >>>:set -XTypeApplications
    --   >>>:set -XDataKinds
    --   >>>:t dims @(Tail '[3,2,5,7])
    --   dims @(Tail '[3,2,5,7]) :: Dims '[2, 5, 7]
    --
    dims :: Dims ds

instance KnownDims ('[] :: [Nat]) where
    dims = U
    {-# INLINE dims #-}

instance (KnownDim d, KnownDims ds) => KnownDims (d :+ ds :: [Nat]) where
    dims = dim :* dims
    {-# INLINE dims #-}

instance KnownDims ds => Reflects ds (Dims ds) where
    reflect _ = dims
