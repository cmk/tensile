{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-} 
module Numeric.Tensile.Dimensions.Types (
  Numeric.TypedList.TypedList(..),
  Numeric.TypedList.snoc,
  Nat(..),
  KnownNat(..),
  natVal,
  SomeNat(..),
  S.Sort(..),
  --module Numeric.Dim,
  module Numeric.Type.Evidence,
  module Numeric.Tensile.Dimensions.Types,
  module Numeric.Type.List
) where

import Data.Proxy
import Numeric.Type.Evidence
import Numeric.TypedList
import Unsafe.Coerce (unsafeCoerce)

import           GHC.Exts           (Constraint)
import GHC.TypeLits
import           Data.Type.Bool
import           Data.Type.Equality

import Numeric.Dim (KnownDim(..))
import Numeric.Dimensions.Dims (Dimensions(..),SomeDims(..))
import Numeric.Type.List -- (type(+:),(+:))

import qualified Data.Singletons.Prelude.List as S (Reverse(..),Sort(..))
import qualified Numeric.Dim as D
import qualified Numeric.Dimensions.Dims as DS


--import qualified  Numeric.Type.List as L
impossible :: a
impossible = error "Numeric.Tensile: impossible"

type x < y = (CmpNat x y) ~ 'LT

type family Rank (xs :: [k]) :: Nat where
    Rank '[] = 0
    Rank (_ ': xs) = 1 + Rank xs

type family Size (xs :: [Nat]) :: Nat where
    Size '[] = 1
    Size (x ': xs) = x * Size xs

type Permutable d d' = (S.Sort d ~ S.Sort d')
type Reshapable d d' = (Size d ~ Size d')

class Reifies s a | s -> a where
  -- | Recover a value inside a 'reify' context, given a proxy for its reified type.
  reflect :: proxy s -> a

{-
type Dim (d :: Nat) = D.Dim d

-- data SomeDims = forall (ns :: [Nat]) . SomeDims (Dims ns)
-- should be > 1
data SomeDim = forall (n :: Nat). SomeDim (Dim n) 
instance Show SomeDim where show (SomeDim d) = show d

-- unsafe should be > 1
someDimVal :: Word -> SomeDim
someDimVal = SomeDim . unsafeCoerce

someDimsVal :: [SomeDim] -> SomeDims
someDimsVal = SomeDims . unsafeCoerce . fmap unsafeCoerce

type Dims (d :: [Nat]) = DS.Dims d
-- type Dims (ds :: [Nat]) = TypedList Dim ds
--
type Pos d = (1 <= d)

type family Positive (xs :: [Nat]) :: Constraint where
    Positive '[] = ()
    Positive (x ': xs) = (Pos x, Positive xs)

-- TODO this is a bit of a hack. Error message is:
-- Couldn't match type ‘'False’ with ‘'True’
--
-- Rewrite KnownDim to enforce positivity.
type KnownDims (ds :: [Nat]) = Dimensions ds -- (Dimensions ds, Positive ds)


instance KnownDim (d :: Nat) => Reifies d (Dim d) where
  reflect _ = dim

instance KnownDims (d :: [Nat]) => Reifies d (Dims d) where
  reflect _ = dims

newtype MagicDim d r = MagicDim (KnownDim d => r)

newtype MagicDim' r = MagicDim' (forall (d :: Nat). KnownDim d => Proxy d -> r)

reifyDim :: forall d r . Dim d -> (KnownDim d => r) -> r
reifyDim d k = unsafeCoerce (MagicDim k :: MagicDim d r) d

reifyDim' :: forall r. Word -> (forall (d :: Nat). KnownDim d => Proxy d -> r) -> r
reifyDim' d k = unsafeCoerce (MagicDim' k :: MagicDim' r) d Proxy

newtype WithKnownDims d r = WithKnownDims (KnownDims d => r)

newtype WithSomeDims r = WithSomeDims (forall (d :: [Nat]). KnownDims d => Proxy d -> r)

reifyDims :: forall d r . Dims d -> (KnownDims d => r) -> r
reifyDims d k = unsafeCoerce (WithKnownDims k :: WithKnownDims d r) d

{-
-- todo : prove reifyDims d == withEvidence (withDims d) 
withDims :: Dims d -> Evidence (KnownDims d)
withDims d = reifyDims d E
-}

reifyDims' :: forall r. [Word] -> (forall (d :: [Nat]). KnownDims d => Proxy d -> r) -> r
reifyDims' d k = unsafeCoerce (WithSomeDims k :: WithSomeDims r) d Proxy

reifySomeDims :: forall r. SomeDims -> (forall (d :: [Nat]). KnownDims d => Proxy d -> r) -> r
reifySomeDims (SomeDims d) k = unsafeCoerce (WithSomeDims k :: WithSomeDims r) d Proxy

-- | A convenience function useful when we need to name a dimensional value multiple times.
withDims :: KnownDims d => (Dims d -> r) -> r
withDims f = f dims

withSomeDims :: forall r. [SomeDim] -> (forall d. Dims d -> r) -> r
withSomeDims d f = case someDimsVal d of SomeDims d' -> f d'

--
-- | A convenience function that names a dimensional value satisfying a certain
-- property.  If the value does not satisfy the property, then the function
-- returns 'Nothing'. 

dimsThat :: KnownDims d => (Dims d -> Bool) -> Maybe (Dims d)
dimsThat p = withDims $ \x -> if p x then Just x else Nothing


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

-- | Get evidence that the two 'KnownDims' lists are actually the "same"
-- list of 'Dim's (that they were instantiated with the same numbers).
--
-- Essentialy runs 'sameDim' over the lists:
--
-- @
-- case 'sameDims' ns ms of
--   Just 'Refl' -> -- in this branch, GHC recognizes that the two ['Dim']s
--                  -- are the same.
--   Nothing   -> -- in this branch, they aren't
-- @
--
-- __Deprecated:__ Use '%~' from /singletons/ instead.
sameDims
    :: Dims ns
    -> Dims ms
    -> Maybe (ns :~: ms)
sameDims = \case
    U      -> \case
      U      -> Just Refl
      _ :* _  -> Nothing
    n :* ns -> \case
      U      -> Nothing
      m :* ms -> do
        Refl <- sameDim n m
        Refl <- sameDims ns ms
        return Refl




-}
-}
