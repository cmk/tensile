{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-} 
{-# LANGUAGE GADTs #-}
module Numeric.Tensile.Dimensions.Dims.Class where

import Data.Functor.Identity
import Data.Proxy
import Unsafe.Coerce (unsafeCoerce)

import GHC.Base (Type)
import           GHC.Exts           (Constraint)
import GHC.TypeLits
import           Data.Type.Bool
import           Data.Type.Equality

import Numeric.Tensile.Dimensions.Dim
import Numeric.Tensile.Dimensions.Types

-- | A list of 'Dim' used to express the shape of a tensor.
type Dims (ds :: [Nat]) = TypedList Dim ds

instance Show (Dims ds) where
    show ds = "Dims " ++ show (dimsVal' ds)
    showsPrec p ds
      = showParen (p >= 10)
      $ showString "Dims " . showsPrec p (dimsVal' ds)

-- | Similar to `natVal` from `GHC.TypeLits`, but returns `Word`.
dimsVal' :: Dims ds -> [Word]
dimsVal' ds = elimDims ds dimVal' --Numeric.Tensile.Dimensions.Types.map dimVal'
{-# INLINE dimsVal' #-}

-- | Product of all dimension sizes.
size' :: Dims ds -> Word
size' = product . dimsVal'
{-# INLINE size' #-}

compareDims' :: Dims as -> Dims bs -> Ordering
compareDims' as bs = compare (dimsVal' as) (dimsVal' bs)
{-# INLINE compareDims' #-}

elimDims :: Dims ds -> (forall d. Dim d -> r) -> [r]
elimDims U _ = []
elimDims (d :* ds) f = f d : elimDims ds f

-------------------------------------------------------------------------------
-- Value Reification
-------------------------------------------------------------------------------

reifyDims :: [Word] -> (forall ds. KnownDims ds => Dims ds -> r) -> Maybe r
reifyDims x f = if all (>0) x then Just $ unsafeReifyDims x f else Nothing

unsafeReifyDims :: [Word] -> (forall ds. KnownDims ds => Dims ds -> r) -> r
unsafeReifyDims []     f = f U
unsafeReifyDims (x:xs) f = 
  unsafeReifyDim x $ \p ->
    unsafeReifyDims xs $ \ps -> f ((reflect p) :* (reflect ps))

reifyDims' :: forall d r . Dims d -> (KnownDims d => r) -> r
reifyDims' d k = unsafeCoerce (WithKnownDims k :: WithKnownDims d r) d

newtype WithKnownDims ds r = WithKnownDims (KnownDims ds => r)

unsafeReifyDims' :: forall r. [Word] -> (forall ds. KnownDims ds => Proxy ds -> r) -> r
unsafeReifyDims' d k = unsafeCoerce (WithSomeDims k :: WithSomeDims r) d Proxy

newtype WithSomeDims r = WithSomeDims (forall ds. KnownDims ds => Proxy ds -> r)

-------------------------------------------------------------------------------
-- Type Reflection 
-------------------------------------------------------------------------------

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

instance (KnownDim d, KnownDims ds) => KnownDims (d :+ ds) where
    dims = dim :* dims
    {-# INLINE dims #-}

instance KnownDims ds => Reflects ds (Dims ds) where
    reflect _ = dims


