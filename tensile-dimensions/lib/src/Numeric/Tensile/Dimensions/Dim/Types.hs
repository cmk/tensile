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
module Numeric.Tensile.Dimensions.Dim.Types (
  Dim(),
  SomeDim(..),
  KnownDim(..),
  dimVal,
  reifyDim,
  reflectDim,
  reflectDim2,
  unsafeReifyDim,
  compareDim,
  sameDim,
  someDim,
  withDim,
  withSomeDim,
  pattern Dim
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

-- | Similar to `natVal` from `GHC.TypeLits`, but returns strictly positive values.
dimVal :: Num n => Dim d -> n
dimVal (DimSing d) = fromIntegral d
{-# INLINE dimVal #-}

-- | Obtain evidence that both values were instantiated with the same 'Nat's.
sameDim :: forall (x :: Nat) (y :: Nat)
         . Dim x -> Dim y -> Maybe (Evidence (x ~ y))
sameDim (DimSing a) (DimSing b)
  | a == b    = Just (unsafeCoerce (E @(x ~ x)))
  | otherwise = Nothing
{-# INLINE sameDim #-}

-- | Ordering of dimension values.
compareDim :: Dim a -> Dim b -> Ordering
compareDim (DimSing a) (DimSing b) = compare a b
{-# INLINE compareDim #-}

{- TODO remove
addDim :: Dim a -> Dim b -> Dim (a + b)
addDim (DimSing a) (DimSing b) = unsafeCoerce (a + b)
{-# INLINE addDim #-}

subDim :: a < b => Dim a -> Dim b -> Dim (a - b)
subDim (DimSing a) (DimSing b) = unsafeCoerce (a - b)
{-# INLINE subDim #-}

mulDim :: Dim a -> Dim b -> Dim (a * b)
mulDim (DimSing a) (DimSing b) = unsafeCoerce (a * b)
{-# INLINE mulDim #-}

expDim :: Dim a -> Dim b -> Dim (a ^ b)
expDim (DimSing a) (DimSing b) = unsafeCoerce (a ^ b)
{-# INLINE expDim #-}

refineDim :: forall d. KnownDim d => (Dim d -> Bool) -> Maybe (Dim d)
refineDim p = reflectDim $ \x -> if p x then Just x else Nothing
{-# INLINE refineDim #-}
-}


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
-- Existential 'Dim' type.
-------------------------------------------------------------------------------

data SomeDim where SomeDim :: KnownDim d => !(Dim d) -> SomeDim

instance Eq SomeDim where
    SomeDim a == SomeDim b = dimVal a == dimVal b

instance Ord SomeDim where
    compare (SomeDim a) (SomeDim b) = compareDim a b

instance Show SomeDim where
    show (SomeDim d) = "SomeDim " ++ show (dimVal d)
    showsPrec p (SomeDim d)
      = showParen (p >= 10)
      $ showString "SomeDim " . showsPrec p (dimVal d)

someDim :: Integral i => i -> Maybe SomeDim
someDim i = if i <= 0 then Nothing else Just sd
  where sd = unsafeReifyDim (fromIntegral i) $ \p -> SomeDim (reflect p)

withSomeDim :: SomeDim -> (forall d. KnownDim d => Dim d -> r) -> r
withSomeDim (SomeDim d) f = f d

-------------------------------------------------------------------------------
-- Value Reification
-------------------------------------------------------------------------------

reifyDim :: forall d r. Dim d -> (KnownDim d => r) -> r
reifyDim d k = unsafeCoerce (WithKnownDim k :: WithKnownDim d r) d

newtype WithKnownDim d r = WithKnownDim (KnownDim d => r)

unsafeReifyDim :: forall r. Word -> (forall d. KnownDim d => Proxy d -> r) -> r
unsafeReifyDim w k = unsafeCoerce (WithSomeDim k :: WithSomeDim r) w Proxy

newtype WithSomeDim r = WithSomeDim (forall d. KnownDim d => Proxy d -> r)

-- @'reifyDim' d == withEvidence ('withDim' d)@ 
withDim :: Dim d -> Evidence (KnownDim d)
withDim d = reifyDim d E
{-# INLINE withDim #-}

-------------------------------------------------------------------------------
-- Type Reflection
-------------------------------------------------------------------------------

-- @d == 'reifyDim' d ('reflectDim' id)@ 
reflectDim :: forall d r. KnownDim d => (Dim d -> r) -> r
reflectDim f = f Dim

reflectDim2 :: forall a b r. (KnownDim a, KnownDim b) => (Dim a -> Dim b -> r) -> r
reflectDim2 f = f Dim Dim

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

instance  KnownDim 1  where { {-# INLINE dim #-}; dim = DimSing 1 }
instance  KnownDim 2  where { {-# INLINE dim #-}; dim = DimSing 2 }
instance  KnownDim 3  where { {-# INLINE dim #-}; dim = DimSing 3 }
instance  KnownDim 4  where { {-# INLINE dim #-}; dim = DimSing 4 }
instance  KnownDim 5  where { {-# INLINE dim #-}; dim = DimSing 5 }
instance  KnownDim 6  where
  { {-# INLINE dim #-}; dim = DimSing 6 }
instance  KnownDim 7  where
  { {-# INLINE dim #-}; dim = DimSing 7 }
instance  KnownDim 8  where
  { {-# INLINE dim #-}; dim = DimSing 8 }
instance  KnownDim 9  where
  { {-# INLINE dim #-}; dim = DimSing 9 }
instance  KnownDim 10 where
  { {-# INLINE dim #-}; dim = DimSing 10 }
instance  KnownDim 11 where
  { {-# INLINE dim #-}; dim = DimSing 11 }
instance  KnownDim 12 where
  { {-# INLINE dim #-}; dim = DimSing 12 }
instance  KnownDim 13 where
  { {-# INLINE dim #-}; dim = DimSing 13 }
instance  KnownDim 14 where
  { {-# INLINE dim #-}; dim = DimSing 14 }
instance  KnownDim 15 where
  { {-# INLINE dim #-}; dim = DimSing 15 }
instance  KnownDim 16 where
  { {-# INLINE dim #-}; dim = DimSing 16 }
instance  KnownDim 17 where
  { {-# INLINE dim #-}; dim = DimSing 17 }
instance  KnownDim 18 where
  { {-# INLINE dim #-}; dim = DimSing 18 }
instance  KnownDim 19 where
  { {-# INLINE dim #-}; dim = DimSing 19 }
instance  KnownDim 20 where
  { {-# INLINE dim #-}; dim = DimSing 20 }
