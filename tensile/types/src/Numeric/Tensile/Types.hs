{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-} 

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
module Numeric.Tensile.Types where

{-(
  Numeric.TypedList.TypedList(..),
  Numeric.TypedList.snoc,
  Dims(..),
  KnownDims(..),
  SomeDims(..),
  someDimsVal,
  listDims,
  dims,
  S.Reverse(..),
  S.Sort(..),
  module Numeric.Dim,
  module Numeric.Type.Evidence,
  module Numeric.Tensile.Types
) where
import Data.Proxy
import qualified Data.Singletons.Prelude.List as S (Reverse(..),Sort(..))
import Numeric.Dim
import Numeric.Dimensions.Dims
import Numeric.Type.Evidence
import Numeric.TypedList
import Unsafe.Coerce (unsafeCoerce)

-}


--import qualified  Numeric.Type.List as L
import           Data.Functor.Identity
import           Data.Kind
import           Data.Proxy
import           Data.Reflection
import           Data.Type.Equality
import           GHC.TypeLits


-- | @'KnownNats' ns@ is intended to represent that every 'Nat' in the
-- type-level list 'ns' is itself a 'KnownNat' (meaning, you can use
-- 'natVal' to get its corresponding 'Integer').
--
-- In practice, just knowing that every item has a 'KnownNat' instance is
-- not enough; it's nice, but unless you're able to "iterate" over every
-- 'Nat' in the list, it's of limited use.  That's why this class also
-- provides a constructor for @'NatList' ns@, so that you can produce
-- a 'NatList' for every @'KnownNat' ns@, which you can iterate over to get
-- @'Proxy' n@s for every 'n' in 'ns' along with the @'KnownNat' n@
-- instances.
--
-- It also has an analogy to 'natVal', 'natsVal', which lets you get a list
-- of the represented 'Integer's for, say, @'Proxy' [1,2,3]@.
--
-- __Deprecated:__ Use 'SingI' from /singletons/ instead.
class KnownNats (ns :: [Nat]) where
    -- | __Deprecated:__ Use 'fromSing' from /singletons/ instead.
    natsVal  :: p ns -> [Integer]
    -- | __Deprecated:__ Use 'sing' from /singletons/ instead.
    natsList :: NatList ns
{-# DEPRECATED KnownNats "Use SingI from the singletons package instead" #-}
{-# DEPRECATED natsVal "Use fromSing from the singletons package instead" #-}
{-# DEPRECATED natsList "Use sing from the singletons package instead" #-}

instance KnownNats '[] where
    natsVal  _ = []
    natsList   = ØNL

instance (KnownNat n, KnownNats ns) => KnownNats (n ': ns) where
    natsVal  _ = natVal (Proxy :: Proxy n) : natsVal (Proxy :: Proxy ns)
    natsList   = Proxy :<# natsList

-- | Represents unknown type-level lists of type-level natural numbers.
-- It's a 'NatList', but you don't know what the list contains at
-- compile-time.
--
-- __Deprecated:__ Use 'SomeSing' from /singletons/ instead.
data SomeNats :: Type where
    SomeNats :: KnownNats ns => !(NatList ns) -> SomeNats
{-# DEPRECATED SomeNats "Use SomeSing from the singletons package instead" #-}

-- | Singleton-esque type for "traversing" over type-level lists of 'Nat's.
-- Essentially contains a (value-level) list of @'Proxy' n@s, but each 'n'
-- has a 'KnownNat' instance for you to use.  At runtime (after type
-- erasure), is more or less equivalent to a @['Integer']@.
--
-- Typically generated using 'natsList'.
--
-- __Deprecated:__ Use 'Sing' from /singletons/ instead.
data NatList :: [Nat] -> Type where
    ØNL   :: NatList '[]
    (:<#) :: (KnownNat n, KnownNats ns)
          => !(Proxy n) -> !(NatList ns) -> NatList (n ': ns)
{-# DEPRECATED NatList "Use Sing from the singletons package instead" #-}

infixr 5 :<#
deriving instance Show (NatList ns)

-- | Utility function for traversing over all of the @'Proxy' n@s in
-- a 'NatList', each with the corresponding 'KnownNat' instance available.
-- Gives the the ability to "change" the represented natural number to
-- a new one, in a 'SomeNat'.
--
-- Can be considered a form of a @Traversal' 'SomeNats' 'SomeNat'@.
traverseNatList
    :: forall f ns. Applicative f
    => (forall n. KnownNat n => Proxy n -> f SomeNat)
    -> NatList ns
    -> f SomeNats
traverseNatList f = go
  where
    go :: forall ms. NatList ms -> f SomeNats
    go = \case
      ØNL      -> pure $ SomeNats ØNL
      n :<# ns -> merge <$> f n <*> go ns
    merge :: SomeNat -> SomeNats -> SomeNats
    merge = \case
      SomeNat n -> \case
        SomeNats ns ->
          SomeNats (n :<# ns)

-- | Like 'traverseNatList', but literally actually a @Traversal'
-- 'SomeNats' 'SomeNat'@, avoiding the Rank-2 types, so is usable with
-- lens-library machinery.
traverseNatList'
    :: forall f. Applicative f
    => (SomeNat -> f SomeNat)
    -> SomeNats
    -> f SomeNats
traverseNatList' f = \case
    SomeNats ns -> traverseNatList (f . SomeNat) ns

-- | Utility function for traversing over all of the @'Proxy' n@s in
-- a 'NatList', each with the corresponding 'KnownNat' instance available.
-- Results are ignored.
traverseNatList_
    :: forall f a ns. Applicative f
    => (forall n. KnownNat n => Proxy n -> f a)
    -> NatList ns
    -> f ()
traverseNatList_ f = go
  where
    go :: forall ms. NatList ms -> f ()
    go = \case
      ØNL       -> pure ()
      n :<# ns -> f n *> go ns

-- | The "eliminator" for 'NatList'.  You can think of this as
-- a dependently typed analogy for a fold.
--
-- /Since 0.2.1.0/
elimNatList
    :: forall p ns. ()
    => p '[]
    -> (forall m ms. (KnownNat m, KnownNats ms) => Proxy m -> p ms -> p (m ': ms))
    -> NatList ns
    -> p ns
elimNatList z s = \case
    ØNL      -> z
    n :<# ns -> s n (elimNatList z s ns)


-- | Utility function for \"mapping\" over each of the 'Nat's in the
-- 'NatList'.
mapNatList
    :: (forall n. KnownNat n => Proxy n -> SomeNat)
    -> NatList ns
    -> SomeNats
mapNatList f = runIdentity . traverseNatList (Identity . f)

-- | Like 'mapNatList', but avoids the Rank-2 types, so can be used with
-- '.' (function composition) and in other situations where 'mapNatList'
-- would cause problems.
mapNatList'
    :: (SomeNat -> SomeNat)
    -> SomeNats
    -> SomeNats
mapNatList' f = runIdentity . traverseNatList' (Identity . f)

-- | List equivalent of 'someNatVal'.  Convert a list of integers into an
-- unknown type-level list of naturals.  Will return 'Nothing' if any of
-- the given 'Integer's is negative.
--
-- __Deprecated:__ Use 'toSing' from /singletons/ instead.
someNatsVal :: [Integer] -> Maybe SomeNats
someNatsVal []     = Just (SomeNats ØNL)
someNatsVal (n:ns) = do
    SomeNat  m  <- someNatVal n
    SomeNats ms <- someNatsVal ns
    return $ SomeNats (m :<# ms)
{-# DEPRECATED someNatsVal "Use toSing from the singletons package instead" #-}

-- | List equivalent of 'reifyNat'.  Given a list of integers, takes
-- a function in an "environment" with a @'NatList' ns@ corresponding to
-- the given list, where every @n@ in @ns@ has a 'KnownNat' instance.
--
-- Essentially a continuation-style version of 'SomeNats'.
--
-- Be aware that this also produces @'KnownNat' n@s where @n@ is negative,
-- without complaining.  To be consistent, within the library, this
-- /should/ be called @reifyNatsPos@; however, the naming choice is for
-- consistency with 'reifyNat' from the /reflections/ package.  Use
-- 'reifyNats'' for a "safe" version.
--
-- __Deprecated:__ Use 'withSomeSing' from /singletons/ instead.
reifyNats :: [Integer] -> (forall ns. KnownNats ns => NatList ns -> r) -> r
reifyNats []     f = f ØNL
reifyNats (n:ns) f = reifyNat n $ \m ->
                       reifyNats ns $ \ms ->
                         f (m :<# ms)
{-# DEPRECATED reifyNats "Use withSomeSing from the singletons package instead" #-}

-- | "Safe" version of 'reifyNats', which will only run the continuation if
-- every 'Integer' in the list is non-negative.  If not, then returns
-- the given "default" value instead.
--
-- __Deprecated:__ Use 'withSomeSing' from /singletons/ instead.
reifyNats'
    :: [Integer]
    -> r
    -> (forall ns. KnownNats ns => NatList ns -> r)
    -> r
reifyNats' ns d f =
    case someNatsVal ns of
      Just (SomeNats ms) -> f ms
      Nothing            -> d
{-# DEPRECATED reifyNats' "Use withSomeSing from the singletons package instead" #-}

-- | Like 'someNatsVal', but will also go ahead and produce 'KnownNat's
-- whose integer values are negative.  It won't ever error on producing
-- them, but extra care must be taken when using the produced 'SomeNat's.
--
-- __Deprecated:__ Use 'toSing' from /singletons/ instead.
someNatsValPos :: [Integer] -> SomeNats
someNatsValPos ns = reifyNats ns SomeNats
{-# DEPRECATED someNatsValPos "Use toSing from the singletons package instead" #-}

-- | Get evidence that the two 'KnownNats' lists are actually the "same"
-- list of 'Nat's (that they were instantiated with the same numbers).
--
-- Essentialy runs 'sameNat' over the lists:
--
-- @
-- case 'sameNats' ns ms of
--   Just 'Refl' -> -- in this branch, GHC recognizes that the two ['Nat']s
--                  -- are the same.
--   Nothing   -> -- in this branch, they aren't
-- @
--
-- __Deprecated:__ Use '%~' from /singletons/ instead.
sameNats
    :: NatList ns
    -> NatList ms
    -> Maybe (ns :~: ms)
sameNats = \case
    ØNL      -> \case
      ØNL      -> Just Refl
      _ :<# _  -> Nothing
    n :<# ns -> \case
      ØNL      -> Nothing
      m :<# ms -> do
        Refl <- sameNat n m
        Refl <- sameNats ns ms
        return Refl
{-# DEPRECATED sameNats "Use (%~) from the singletons package instead" #-}



impossible :: a
impossible = error "Numeric.Tensile: impossible"


{-

type family Rank (xs :: [k]) :: Nat where
    Rank '[] = 0
    Rank (_ ': xs) = 1 + Rank xs

type family Size (xs :: [Nat]) :: Nat where
    Size '[] = 1
    Size (x ': xs) = x * Size xs

type KnownDims = Dimensions

type Permutable d d' = (S.Sort d ~ S.Sort d')
type Reshapable d d' = (Size d ~ Size d')

class Reifies s a | s -> a where
  -- | Recover a value inside a 'reify' context, given a proxy for its reified type.
  reflect :: proxy s -> a

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

newtype WithUnknownDims r = WithUnknownDims (forall (d :: [Nat]). KnownDims d => Proxy d -> r)

reifyDims :: forall d r . Dims d -> (KnownDims d => r) -> r
reifyDims d k = unsafeCoerce (WithKnownDims k :: WithKnownDims d r) d

{-
-- todo : prove reifyDims d == withEvidence (withDims d) 
withDims :: Dims d -> Evidence (KnownDims d)
withDims d = reifyDims d E
-}

reifyDims' :: forall r. [Word] -> (forall (d :: [Nat]). KnownDims d => Proxy d -> r) -> r
reifyDims' d k = unsafeCoerce (WithUnknownDims k :: WithUnknownDims r) d Proxy

reifySomeDims :: forall r. SomeDims -> (forall (d :: [Nat]). KnownDims d => Proxy d -> r) -> r
reifySomeDims (SomeDims d) k = unsafeCoerce (WithUnknownDims k :: WithUnknownDims r) d Proxy

-- | A convenience function useful when we need to name a dimensional value multiple times.
withDims :: KnownDims d => (Dims d -> r) -> r
withDims f = f dims

withSomeDims :: forall r. [Word]
             -> (forall (d :: [Nat]). Dims d -> r)
             -> r
withSomeDims d f =
  case someDimsVal d of
    SomeDims d' -> f d'
 
--
-- | A convenience function that names a dimensional value satisfying a certain
-- property.  If the value does not satisfy the property, then the function
-- returns 'Nothing'. 

dimsThat :: KnownDims d => (Dims d -> Bool) -> Maybe (Dims d)
dimsThat p = withDims $ \x -> if p x then Just x else Nothing
-}

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


-}
