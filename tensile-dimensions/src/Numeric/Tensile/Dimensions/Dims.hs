{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-} 
{-# LANGUAGE GADTs #-}
module Numeric.Tensile.Dimensions.Dims (
  module Numeric.Tensile.Dimensions.Dims,
  module Numeric.Tensile.Dimensions.Dims.Class
) where

import Data.Functor.Identity
import Data.Proxy
import Unsafe.Coerce (unsafeCoerce)

import GHC.Base (Type)
import           GHC.Exts           (Constraint)
import GHC.TypeLits
import           Data.Type.Bool
import           Data.Type.Equality

import Numeric.Tensile.Dimensions.Dim
import Numeric.Tensile.Dimensions.Dims.Class
import Numeric.Tensile.Dimensions.Types

-- TODO push this out everywhere
-- | A convenience function useful when we need to name a dimensional value multiple times.
reflectDims :: forall ds r. KnownDims ds => (Dims ds -> r) -> r
reflectDims f = f (dims @ds)
{-
-- todo : prove reifyDims d == withEvidence (withDims d) 
withDims :: Dims d -> Evidence (KnownDims d)
withDims d = reifyDims d E
-}

reflectDims2 :: forall as bs r. (KnownDims as, KnownDims bs) => (Dims as -> Dims bs -> r) -> r
reflectDims2 f = f (dims @as) (dims @bs)


-- | Similar to `natVal` from `GHC.TypeLits`, but returns `Word`.
fromDims :: forall ds. KnownDims ds => [Word]
fromDims = reflectDims @ds fromDims'
{-# INLINE fromDims #-}

-- | Product of all dimension sizes.
size :: forall ds . KnownDims ds => Word
size = reflectDims @ds size'
{-# INLINE size #-}

compareDims :: forall as bs. (KnownDims as, KnownDims bs) => Dims as -> Dims bs -> Ordering
compareDims as bs = reflectDims2 @as @bs compareDims'
{-# INLINE compareDims #-}

-- | A convenience function that names a dimensional value satisfying a certain
-- property.  If the value does not satisfy the property, then the function
-- returns 'Nothing'. 

refineDims :: forall ds. KnownDims ds => (Dims ds -> Bool) -> Maybe (Dims ds)
refineDims p = reflectDims $ \x -> if p x then Just x else Nothing

-------------------------------------------------------------------------------
-- Existential 'Dims' type.
-------------------------------------------------------------------------------

data SomeDims where SomeDims :: KnownDims ds => !(Dims ds) -> SomeDims

instance Eq SomeDims where
  SomeDims as == SomeDims bs = fromDims' as == fromDims' bs

instance Ord SomeDims where
  compare (SomeDims as) (SomeDims bs) = compareDims' as bs

instance Show SomeDims where
  show (SomeDims ds) = "SomeDim " ++ show (fromDims' ds)
  showsPrec p (SomeDims ds)
    = showParen (p >= 10)
    $ showString "SomeDims " . showsPrec p (fromDims' ds)

someDims :: [Word] -> Maybe [SomeDim]
someDims = traverse someDim
{-# INLINE someDims #-}

withSomeDims :: [SomeDim] -> (forall ds. KnownDims ds => Dims ds -> r) -> r 
withSomeDims []     f = f U
withSomeDims (x:xs) f = withSomeDim x $ \d ->
                          withSomeDims xs $ \ds -> f (d :* ds)
{-

TODO: Add prop tests
> ds = dims @'[1,2,3]
> traverseDims (pure . SomeDim) ds
[SomeDim 1,SomeDim 2,SomeDim 3]

> sd = fromJust $ someDims [1,2,3]
> traverseDims' pure sd
[SomeDim 1,SomeDim 2,SomeDim 3]

> ds = dims @'[1,2,3]
> mapDims SomeDim ds
[SomeDim 1,SomeDim 2,SomeDim 3]

> sd = fromJust $ someDims [1,2,3]
> traverseDims' pure sd
[SomeDim 1,SomeDim 2,SomeDim 3]

traverse (\s -> pure $ withSomeDim s fromDim') sd == withSomeDims sd fromDims'

-}

-- | Utility function for traversing over all of the @'Dim' d@s in
-- a 'Dims', each with the corresponding 'KnownDim' instance available.
-- Gives the the ability to "change" the represented natural number to
-- a new one, in a 'SomeDim'.
--
-- Can be considered a form of a @Traversal' 'SomeDims' 'SomeDim'@.
traverseDims
  :: forall f ds. Applicative f
  => (forall d. KnownDim d => Dim d -> f SomeDim) -> Dims ds -> f [SomeDim]
traverseDims k = go
  where
    go :: forall ds. Dims ds -> f [SomeDim]
    go = \case
      U      -> pure []
      d :* ds -> (:) <$> withEvidence (dimEv d) (k d) <*> go ds
 
-- | Like 'traverseDims', but with type @Traversal' 'SomeDims' 'SomeDim'@, 
-- thus avoiding Rank-2 types. Usable with lens-library machinery.
traverseDims'
  :: forall f. Applicative f
  => (SomeDim -> f SomeDim) -> [SomeDim] -> f [SomeDim]
traverseDims' k = traverse (\s -> withSomeDim s (k . SomeDim))

-- | Utility function for \"mapping\" over each of the 'Dim's in the
-- 'Dims'.
mapDims :: (forall d. KnownDim d => Dim d -> SomeDim) -> Dims ds -> [SomeDim]
mapDims f = runIdentity . traverseDims (Identity . f)

-- | Like 'mapDims', but without Rank-2 types. Usable with '.' 
-- (function composition) and in situations where 'mapDims' would cause problems.
mapDims' :: (SomeDim -> SomeDim) -> [SomeDim] -> [SomeDim]
mapDims' f = runIdentity . traverseDims' (Identity . f)


--reifySomeDims :: forall r. SomeDims -> (forall (d :: [Nat]). KnownDims d => Proxy d -> r) -> r
--reifySomeDims (SomeDims d) k = unsafeCoerce (WithSomeDims k :: WithSomeDims r) d Proxy

--withSomeDims :: forall r. SomeDims -> (forall ds. KnownDims ds => Dims ds -> r) -> r
--withSomeDims d f = case someDims d of SomeDims d' -> f d'

--foo :: KnownDims '[1,2] => [Word]
--foo = withDims fromDims'

{-
-- | The "eliminator" for 'Dims'.  You can think of this as
-- a dependently typed analogy for a fold.
foldDims
    :: forall p ns. ()
    => p '[]
    -> (forall m ms. (KnownDim m, KnownDims ms) => Proxy m -> p ms -> p (m ': ms))
    -> Dims ns
    -> p ns
foldDims z s = \case
    U      -> z
    n :* ns -> s n (foldDims z s ns)
-}



