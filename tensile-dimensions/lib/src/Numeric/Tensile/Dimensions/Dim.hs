{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE LambdaCase           #-}

{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE UndecidableInstances   #-} 
{-# LANGUAGE ViewPatterns           #-}
module Numeric.Tensile.Dimensions.Dim (
  module Numeric.Tensile.Dimensions.Dim,
  module Numeric.Tensile.Dimensions.Dim.Types
) where

import GHC.TypeLits
import Numeric.Tensile.Dimensions.Types (Reflects(..), type (<))
import Numeric.Tensile.Dimensions.Dim.Types
import Numeric.Type.Evidence

reflectDim :: forall d r. KnownDim d => (Dim d -> r) -> r
reflectDim f = f Dim

reflectDim2 :: forall a b r. (KnownDim a, KnownDim b) => (Dim a -> Dim b -> r) -> r
reflectDim2 f = f Dim Dim

{-
reifyDim d == withEvidence (withDim d)

reifyReflect :: Dim d -> Dim d
reifyReflect d = reifyDim d (reflectDim id)
-}

refineDim :: forall d. KnownDim d => (Dim d -> Bool) -> Maybe (Dim d)
refineDim p = reflectDim $ \x -> if p x then Just x else Nothing

-- | Similar to `natVal` from `GHC.TypeLits`, but returns `Word`.
fromDim :: forall d. KnownDim d => Word
fromDim = reflectDim @d fromDim'
{-# INLINE fromDim #-}

-- | We either get evidence that this function
--   was instantiated with the same type-level numbers, or Nothing.
sameDim :: forall a b. (KnownDim a, KnownDim b) => Maybe (Evidence (a ~ b))
sameDim = reflectDim2 sameDim'
{-# INLINE sameDim #-}

-- | Ordering of dimension values.
compareDim :: forall a b. (KnownDim a, KnownDim b) => Ordering
compareDim = reflectDim2 @a @b compareDim'
{-# INLINE compareDim #-}

addDim :: forall a b. (KnownDim a, KnownDim b) => Dim (a + b)
addDim = reflectDim2 @a @b addDim'

subDim :: forall a b. (KnownDim a, KnownDim b, a < b) => Dim (a - b)
subDim = reflectDim2 @a @b subDim'
{-# INLINE subDim #-}

mulDim :: forall a b. (KnownDim a, KnownDim b) => Dim (a * b)
mulDim = reflectDim2 @a @b mulDim'
{-# INLINE mulDim #-}

expDim :: forall a b. (KnownDim a, KnownDim b) => Dim (a ^ b)
expDim = reflectDim2 @a @b expDim'
{-# INLINE expDim #-}

-------------------------------------------------------------------------------
-- Existential 'Dim' type.
-------------------------------------------------------------------------------

data SomeDim where SomeDim :: KnownDim d => !(Dim d) -> SomeDim

instance Eq SomeDim where
    SomeDim a == SomeDim b = fromDim' a == fromDim' b

instance Ord SomeDim where
    compare (SomeDim a) (SomeDim b) = compareDim' a b

instance Show SomeDim where
    show (SomeDim d) = "SomeDim " ++ show (fromDim' d)
    showsPrec p (SomeDim d)
      = showParen (p >= 10)
      $ showString "SomeDim " . showsPrec p (fromDim' d)

someDim :: Word -> Maybe SomeDim
someDim w = if w == 0 then Nothing else Just sd
  where sd = unsafeReifyDim w $ \p -> SomeDim (reflect p)

withSomeDim :: SomeDim -> (forall d. KnownDim d => Dim d -> r) -> r
withSomeDim (SomeDim d) f = f d
