{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE UndecidableInstances   #-} 
{-# LANGUAGE ViewPatterns           #-}
module Numeric.Tensile.Dimensions.Dim.Types (
  Dim(),
  pattern Dim,
  KnownDim(..),
  fromDim',
  withDim,
  compareDim',
  sameDim',
  addDim',
  subDim',
  mulDim',
  expDim',
  reifyDim,
  unsafeReifyDim
) where

import Data.Proxy
import Data.Kind (Type)
import Data.Type.Bool
import Data.Type.Equality
import GHC.Exts (Constraint, Proxy#, proxy#)
import GHC.TypeLits
import Numeric.Type.Evidence
import Unsafe.Coerce (unsafeCoerce)

import Numeric.Tensile.Dimensions.Types (Reflects(..), type (<))


-- | Singleton type to store type-level dimension value.
-- Dimensions are restricted to strictly positive naturals.
newtype Dim (d :: Nat) = DimSing Word deriving (Eq, Ord)

instance Show (Dim d) where
    show (DimSing w) = "Dim " ++ show w
    showsPrec p (DimSing w)
      = showParen (p >= 10)
      $ showString "Dim " . showsPrec p w

-- | Similar to `natVal` from `GHC.TypeLits`, but returns `Word`.
fromDim' :: Dim d -> Word
fromDim' (DimSing w) = w
{-# INLINE fromDim' #-}

-- @'reifyDim' d == withEvidence ('withDim' d)@ 
withDim :: Dim d -> Evidence (KnownDim d)
withDim d = reifyDim d E
{-# INLINE withDim #-}

--  Match against this pattern to bring a `KnownDim` instance into scope.
pattern Dim :: forall d. KnownDim d => Dim d
pattern Dim <- (withDim -> E)
  where
    Dim = dim @d
-- Starting from GHC 8.2, compiler supports specifying lists of complete
-- pattern synonyms.
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE Dim #-}
#endif

-------------------------------------------------------------------------------
-- Arithmetic
-------------------------------------------------------------------------------

-- | We either get evidence that this function
--   was instantiated with the same type-level numbers, or Nothing.
--
--   Note, this function works on @Nat@-indexed dimensions only,
--   because @Dim (XN x)@ does not have runtime evidence to infer @x@
--   and `KnownDim x` does not imply `KnownDim (XN x)`.
sameDim' :: forall (x :: Nat) (y :: Nat)
         . Dim x -> Dim y -> Maybe (Evidence (x ~ y))
sameDim' (DimSing a) (DimSing b)
  | a == b    = Just (unsafeCoerce (E @(x ~ x)))
  | otherwise = Nothing
{-# INLINE sameDim' #-}

-- | Ordering of dimension values.
compareDim' :: Dim a -> Dim b -> Ordering
compareDim' (DimSing a) (DimSing b) = compare a b
{-# INLINE compareDim' #-}

addDim' :: Dim a -> Dim b -> Dim (a + b)
addDim' (DimSing a) (DimSing b) = unsafeCoerce (a + b)
{-# INLINE addDim' #-}

subDim' :: a < b => Dim a -> Dim b -> Dim (a - b)
subDim' (DimSing a) (DimSing b) = unsafeCoerce (a - b)
{-# INLINE subDim' #-}

mulDim' :: Dim a -> Dim b -> Dim (a * b)
mulDim' (DimSing a) (DimSing b) = unsafeCoerce (a * b)
{-# INLINE mulDim' #-}

expDim' :: Dim a -> Dim b -> Dim (a ^ b)
expDim' (DimSing a) (DimSing b) = unsafeCoerce (a ^ b)
{-# INLINE expDim' #-}

-------------------------------------------------------------------------------
-- Value Reification
-------------------------------------------------------------------------------

reifyDim :: forall d r. Dim d -> (KnownDim d => r) -> r
reifyDim d k = unsafeCoerce (WithKnownDim k :: WithKnownDim d r) d

newtype WithKnownDim d r = WithKnownDim (KnownDim d => r)

unsafeReifyDim :: forall r. Word -> (forall d. KnownDim d => Proxy d -> r) -> r
unsafeReifyDim w k = unsafeCoerce (WithSomeDim k :: WithSomeDim r) w Proxy

newtype WithSomeDim r = WithSomeDim (forall d. KnownDim d => Proxy d -> r)

-------------------------------------------------------------------------------
-- Type Reflection
-------------------------------------------------------------------------------

-- TODO dont export dim itself?
-- | This class provides the `Dim` associated with a type-level natural,
class KnownDim d where
    -- | Get value of type-level dim at runtime.
    --
    --   Note, this function is supposed to be used with @TypeApplications@,
    --   and the @KnownDim@ class has varying kind of the parameter;
    --   thus, the function has two type paremeters (kind and type of @n@).
    --   For example, you can type:
    --
    --   >>>:set -XTypeApplications
    --   >>>:set -XDataKinds
    --   >>>:t dim @3
    --   dim @3 :: Dim 3
    --
    --   >>>:set -XTypeOperators
    --   >>>:t dim @(13 - 6)
    --   dim @(13 - 6) :: Dim 7
    dim :: Dim d

instance KnownDim d => Reflects d (Dim d) where reflect _ = dim

instance {-# OVERLAPPABLE #-} (KnownNat d, 1 <= d) => KnownDim d where
    {-# INLINE dim #-}
    dim = DimSing (fromInteger (natVal' (proxy# :: Proxy# d)))

instance {-# OVERLAPPING #-} KnownDim 1  where
  { {-# INLINE dim #-}; dim = DimSing 1 }
instance {-# OVERLAPPING #-} KnownDim 2  where
  { {-# INLINE dim #-}; dim = DimSing 2 }
instance {-# OVERLAPPING #-} KnownDim 3  where
  { {-# INLINE dim #-}; dim = DimSing 3 }
instance {-# OVERLAPPING #-} KnownDim 4  where
  { {-# INLINE dim #-}; dim = DimSing 4 }
instance {-# OVERLAPPING #-} KnownDim 5  where
  { {-# INLINE dim #-}; dim = DimSing 5 }
instance {-# OVERLAPPING #-} KnownDim 6  where
  { {-# INLINE dim #-}; dim = DimSing 6 }
instance {-# OVERLAPPING #-} KnownDim 7  where
  { {-# INLINE dim #-}; dim = DimSing 7 }
instance {-# OVERLAPPING #-} KnownDim 8  where
  { {-# INLINE dim #-}; dim = DimSing 8 }
instance {-# OVERLAPPING #-} KnownDim 9  where
  { {-# INLINE dim #-}; dim = DimSing 9 }
instance {-# OVERLAPPING #-} KnownDim 10 where
  { {-# INLINE dim #-}; dim = DimSing 10 }
instance {-# OVERLAPPING #-} KnownDim 11 where
  { {-# INLINE dim #-}; dim = DimSing 11 }
instance {-# OVERLAPPING #-} KnownDim 12 where
  { {-# INLINE dim #-}; dim = DimSing 12 }
instance {-# OVERLAPPING #-} KnownDim 13 where
  { {-# INLINE dim #-}; dim = DimSing 13 }
instance {-# OVERLAPPING #-} KnownDim 14 where
  { {-# INLINE dim #-}; dim = DimSing 14 }
instance {-# OVERLAPPING #-} KnownDim 15 where
  { {-# INLINE dim #-}; dim = DimSing 15 }
instance {-# OVERLAPPING #-} KnownDim 16 where
  { {-# INLINE dim #-}; dim = DimSing 16 }
instance {-# OVERLAPPING #-} KnownDim 17 where
  { {-# INLINE dim #-}; dim = DimSing 17 }
instance {-# OVERLAPPING #-} KnownDim 18 where
  { {-# INLINE dim #-}; dim = DimSing 18 }
instance {-# OVERLAPPING #-} KnownDim 19 where
  { {-# INLINE dim #-}; dim = DimSing 19 }
instance {-# OVERLAPPING #-} KnownDim 20 where
  { {-# INLINE dim #-}; dim = DimSing 20 }
