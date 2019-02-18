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
  module Numeric.Tensile.Dimensions.Dim.Class
) where

import Data.Proxy
import Numeric.Type.Evidence
import Unsafe.Coerce (unsafeCoerce)

import Data.Kind (Type)
import           GHC.Exts           (Constraint, Proxy#, proxy#)
--import           GHC.Exts           (Constraint)
--import GHC.TypeLits
import           Data.Type.Bool
import           Data.Type.Equality
import           GHC.TypeLits       as TL

--import Numeric.Tensile.Dimensions.Dim.Class
import Numeric.Tensile.Dimensions.Types (Reifies(..), type (<))
import Numeric.Tensile.Dimensions.Dim.Class


reflectDim :: forall d r. KnownDim d => (Dim d -> r) -> r
reflectDim f = f Dim

reflectDim2 :: forall a b r. (KnownDim a, KnownDim b) => (Dim a -> Dim b -> r) -> r
reflectDim2 f = f Dim Dim

refineDim :: forall d. KnownDim d => (Dim d -> Bool) -> Maybe (Dim d)
refineDim p = reflectDim $ \x -> if p x then Just x else Nothing

-- | Similar to `natVal` from `GHC.TypeLits`, but returns `Word`.
dimVal :: forall d. KnownDim d => Word
dimVal = reflectDim @d dimVal'
{-# INLINE dimVal #-}

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

--data SomeDim = forall d. SomeDim (Dim d)

data SomeDim where SomeDim :: KnownDim d => !(Dim d) -> SomeDim

instance Eq SomeDim where
  SomeDim a == SomeDim b = dimVal' a == dimVal' b

instance Ord SomeDim where
  compare (SomeDim a) (SomeDim b) = compareDim' a b

instance Show SomeDim where
  show (SomeDim d) = "SomeDims " ++ show (dimVal' d)
  showsPrec p (SomeDim d)
    = showParen (p >= 10)
    $ showString "SomeDims " . showsPrec p (dimVal' d)

someDim :: Word -> Maybe SomeDim
someDim w = reifyDim w SomeDim

withSomeDim :: SomeDim -> (forall d. KnownDim d => Dim d -> r) -> r
withSomeDim (SomeDim d) f = f d

{-
compareDims' :: Dims as -> Dims bs -> Ordering
compareDims' a b = compare (listDims a) (listDims b)
{-# INLINE compareDims #-}

compareDims :: forall as bs p q
              . (Dimensions as, Dimensions bs)
             => p as -> q bs -> Ordering
compareDims _ _ = compareDims' (dims @_ @as) (dims @_ @bs)

fromDims = listDims
compareDims
sameDims
totalDim

-- data SomeDims = forall (ns :: [Nat]) . SomeDims (Dims ns)
-- should be > 1


someDimsVal :: [SomeDim] -> SomeDims
someDimsVal = SomeDims . unsafeCoerce . fmap unsafeCoerce


http://hackage.haskell.org/package/typelits-witnesses-0.3.0.3/docs/src/GHC.TypeLits.List.html#KnownNats

{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-} 
import           Data.Functor.Identity
import           Data.Kind
import           Data.Proxy
import           Data.Reflection
import           Data.Type.Equality
import           GHC.TypeLits


-- | @'KnownDims' ns@ is intended to represent that every 'Dim' in the
-- type-level list 'ns' is itself a 'KnownDim' (meaning, you can use
-- 'natVal' to get its corresponding 'Word').
--
-- In practice, just knowing that every item has a 'KnownDim' instance is
-- not enough; it's nice, but unless you're able to "iterate" over every
-- 'Dim' in the list, it's of limited use.  That's why this class also
-- provides a constructor for @'Dims' ns@, so that you can produce
-- a 'Dims' for every @'KnownDim' ns@, which you can iterate over to get
-- @'Proxy' n@s for every 'n' in 'ns' along with the @'KnownDim' n@
-- instances.
--
-- It also has an analogy to 'natVal', 'natsVal', which lets you get a list
-- of the represented 'Word's for, say, @'Proxy' [1,2,3]@.
--
-- __Deprecated:__ Use 'SingI' from /singletons/ instead.
class KnownDims (ns :: [Nat]) where
    -- | __Deprecated:__ Use 'fromSing' from /singletons/ instead.
    natsVal  :: p ns -> [Word]
    -- | __Deprecated:__ Use 'sing' from /singletons/ instead.
    natsList :: Dims ns

instance KnownDims '[] where
    natsVal  _ = []
    natsList   = U

instance (KnownDim n, KnownDims ns) => KnownDims (n ': ns) where
    natsVal  _ = natVal (Proxy :: Proxy n) : natsVal (Proxy :: Proxy ns)
    natsList   = Proxy :* natsList

-- | Represents unknown type-level lists of type-level natural numbers.
-- It's a 'Dims', but you don't know what the list contains at
-- compile-time.
--
-- __Deprecated:__ Use 'SomeSing' from /singletons/ instead.
data SomeDims :: Type where
    SomeDims :: KnownDims ns => !(Dims ns) -> SomeDims

-- | Same as SomeNat, but for Dimensions:
--   Hide all information about Dimensions inside
data SomeDims = forall (ns :: [Nat]) . SomeDims (Dims ns)


-- | Singleton-esque type for "traversing" over type-level lists of 'Dim's.
-- Essentially contains a (value-level) list of @'Proxy' n@s, but each 'n'
-- has a 'KnownDim' instance for you to use.  At runtime (after type
-- erasure), is more or less equivalent to a @['Word']@.
--
-- Typically generated using 'natsList'.
--
-- __Deprecated:__ Use 'Sing' from /singletons/ instead.
data Dims :: [Nat] -> Type where
    U   :: Dims '[]
    (:*) :: (KnownDim n, KnownDims ns)
          => !(Proxy n) -> !(Dims ns) -> Dims (n ': ns)

infixr 5 :*
deriving instance Show (Dims ns)

-- | Utility function for traversing over all of the @'Proxy' n@s in
-- a 'Dims', each with the corresponding 'KnownDim' instance available.
-- Gives the the ability to "change" the represented natural number to
-- a new one, in a 'SomeDim'.
--
-- Can be considered a form of a @Traversal' 'SomeDims' 'SomeDim'@.
traverseDims
    :: forall f ns. Applicative f
    => (forall n. KnownDim n => Proxy n -> f SomeDim)
    -> Dims ns
    -> f SomeDims
traverseDims f = go
  where
    go :: forall ms. Dims ms -> f SomeDims
    go = \case
      U      -> pure $ SomeDims U
      n :* ns -> merge <$> f n <*> go ns
    merge :: SomeDim -> SomeDims -> SomeDims
    merge = \case
      SomeDim n -> \case
        SomeDims ns ->
          SomeDims (n :* ns)

-- | Like 'traverseDims', but literally actually a @Traversal'
-- 'SomeDims' 'SomeDim'@, avoiding the Rank-2 types, so is usable with
-- lens-library machinery.
traverseDims'
    :: forall f. Applicative f
    => (SomeDim -> f SomeDim)
    -> SomeDims
    -> f SomeDims
traverseDims' f = \case
    SomeDims ns -> traverseDims (f . SomeDim) ns

-- | Utility function for traversing over all of the @'Proxy' n@s in
-- a 'Dims', each with the corresponding 'KnownDim' instance available.
-- Results are ignored.
traverseDims_
    :: forall f a ns. Applicative f
    => (forall n. KnownDim n => Proxy n -> f a)
    -> Dims ns
    -> f ()
traverseDims_ f = go
  where
    go :: forall ms. Dims ms -> f ()
    go = \case
      U       -> pure ()
      n :* ns -> f n *> go ns

-- | The "eliminator" for 'Dims'.  You can think of this as
-- a dependently typed analogy for a fold.
--
-- /Since 0.2.1.0/
elimDims
    :: forall p ns. ()
    => p '[]
    -> (forall m ms. (KnownDim m, KnownDims ms) => Proxy m -> p ms -> p (m ': ms))
    -> Dims ns
    -> p ns
elimDims z s = \case
    U      -> z
    n :* ns -> s n (elimDims z s ns)


-- | Utility function for \"mapping\" over each of the 'Dim's in the
-- 'Dims'.
mapDims
    :: (forall n. KnownDim n => Proxy n -> SomeDim)
    -> Dims ns
    -> SomeDims
mapDims f = runIdentity . traverseDims (Identity . f)

-- | Like 'mapDims', but avoids the Rank-2 types, so can be used with
-- '.' (function composition) and in other situations where 'mapDims'
-- would cause problems.
mapDims'
    :: (SomeDim -> SomeDim)
    -> SomeDims
    -> SomeDims
mapDims' f = runIdentity . traverseDims' (Identity . f)

-- | List equivalent of 'someDimVal'.  Convert a list of integers into an
-- unknown type-level list of naturals.  Will return 'Nothing' if any of
-- the given 'Word's is negative.
--
-- __Deprecated:__ Use 'toSing' from /singletons/ instead.
someDimsVal :: [Word] -> Maybe SomeDims
someDimsVal []     = Just (SomeDims U)
someDimsVal (n:ns) = do
    SomeDim  m  <- someDimVal n
    SomeDims ms <- someDimsVal ns
    return $ SomeDims (m :* ms)

-- | List equivalent of 'reifyDim'.  Given a list of integers, takes
-- a function in an "environment" with a @'Dims' ns@ corresponding to
-- the given list, where every @n@ in @ns@ has a 'KnownDim' instance.
--
-- Essentially a continuation-style version of 'SomeDims'.
--
-- Be aware that this also produces @'KnownDim' n@s where @n@ is negative,
-- without complaining.  To be consistent, within the library, this
-- /should/ be called @reifyDimsNat@; however, the naming choice is for
-- consistency with 'reifyDim' from the /reflections/ package.  Use
-- 'reifyDims'' for a "safe" version.
--
-- __Deprecated:__ Use 'withSomeSing' from /singletons/ instead.
reifyDims :: [Word] -> (forall ns. KnownDims ns => Dims ns -> r) -> r
reifyDims []     f = f U
reifyDims (n:ns) f = reifyDim n $ \m ->
                       reifyDims ns $ \ms ->
                         f (m :* ms)

-- | "Safe" version of 'reifyDims', which will only run the continuation if
-- every 'Word' in the list is non-negative.  If not, then returns
-- the given "default" value instead.
--
-- __Deprecated:__ Use 'withSomeSing' from /singletons/ instead.
reifyDims'
    :: [Word]
    -> r
    -> (forall ns. KnownDims ns => Dims ns -> r)
    -> r
reifyDims' ns d f =
    case someDimsVal ns of
      Just (SomeDims ms) -> f ms
      Nothing            -> d

-- | Like 'someDimsVal', but will also go ahead and produce 'KnownDim's
-- whose integer values are negative.  It won't ever error on producing
-- them, but extra care must be taken when using the produced 'SomeDim's.
--
-- __Deprecated:__ Use 'toSing' from /singletons/ instead.
someDimsValNat :: [Word] -> SomeDims
someDimsValNat ns = reifyDims ns SomeDims


-}
