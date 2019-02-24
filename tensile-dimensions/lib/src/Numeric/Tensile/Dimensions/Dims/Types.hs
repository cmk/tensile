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
import Data.Int (Int64)
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
    (==) = unsafeCoerce ((==) :: [Int64] -> [Int64] -> Bool)
    {-# INLINE (==) #-}

instance Show (Dims ds) where
    show ds = "Dims " ++ show (listDims ds)
    showsPrec p ds = showParen (p >= 10)
      $ showString "Dims " . showsPrec p (listDims ds)

-- | Similar to `natVal` from `GHC.TypeLits`, but returns `Int64`.
listDims :: Dims ds -> [Int64]
listDims ds = listVals ds dimVal --Numeric.Tensile.Dimensions.Types.map dimVal
{-# INLINE listDims #-}

-- | Product of all dimension sizes. 
-- Caution: numerical overflow will lead to undefined behavior.
size :: Dims ds -> Int64
size = product . listDims
{-# INLINE size #-}

compareDims :: Dims as -> Dims bs -> Ordering
compareDims as bs = compare (listDims as) (listDims bs)
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
patKDims S = PatKDims
patKDims (Dim :+ ns) = case patKDims ns of
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

someDims :: Integral i => [i] -> Maybe SomeDims
someDims = traverse someDim
{-# INLINE someDims #-}

-- @'withSomeDims' ds 'listDims' == runIdentity . traverse (\s -> Identity $ Numeric.Tensile.Dimensions.Dim.Types.withSomeDim s Numeric.Tensile.Dimensions.Dim.Types.dimVal) $ ds@ 
withSomeDims :: SomeDims -> (forall ds. KnownDims ds => Dims ds -> r) -> r 
withSomeDims []     f = f S
withSomeDims (x:xs) f = withSomeDim x $ \d ->
                          withSomeDims xs $ \ds -> f (d :+ ds)
{-# INLINE withSomeDims #-}

-------------------------------------------------------------------------------
-- Value Reification
-------------------------------------------------------------------------------

reifyDims :: forall d r . Dims d -> (KnownDims d => r) -> r
reifyDims d k = unsafeCoerce (WithKnownDims k :: WithKnownDims d r) d

newtype WithKnownDims ds r = WithKnownDims (KnownDims ds => r)

unsafeReifyDims :: [Int64] -> (forall ds. KnownDims ds => Dims ds -> r) -> r
unsafeReifyDims []     f = f S
unsafeReifyDims (x:xs) f = 
  unsafeReifyDim x $ \p ->
    unsafeReifyDims xs $ \ps -> f ((reflect p) :+ (reflect ps))

-- @'reifyDims' ds == withEvidence ('withDims' ds)@ 
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
    dims = S
    {-# INLINE dims #-}

instance (KnownDim d, KnownDims ds) => KnownDims (d :+ ds :: [Nat]) where
    dims = dim :+ dims
    {-# INLINE dims #-}

instance KnownDims ds => Reflects ds (Dims ds) where
    reflect _ = dims
