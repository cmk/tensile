{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-} 
{-# LANGUAGE PatternSynonyms        #-}

{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE ViewPatterns           #-}

module Numeric.Tensile.Dimensions.Types (
  module Numeric.Tensile.Dimensions.Types,
  module Numeric.Type.Evidence,
  module Numeric.Type.List,
  Nat(..),
  KnownNat(..),
  natVal,
  SomeNat(..),
  S.Sort(..)
) where

import Control.Arrow         (first)
import Data.Proxy
import Data.Type.Bool
import Data.Type.Equality
import GHC.Base              (Type)
import GHC.Exts              
import GHC.TypeLits
import Numeric.Type.Evidence
import Numeric.Type.List  -- (type(+:),(+:))
import Unsafe.Coerce         (unsafeCoerce)
import qualified Data.Singletons.Prelude.List as S (Sort(..))

impossible :: a
impossible = error "Numeric.Tensile: impossible"

type x < y = (CmpNat x y) ~ 'LT

type family Size (ds :: [Nat]) :: Nat where
    Size '[] = 1
    Size (x ': ds) = x * Size ds

type Reshapable d d' = (Size d ~ Size d')

type family Rank (ds :: [Nat]) :: Nat where
    Rank '[] = 0
    Rank (_ ': ds) = 1 + Rank ds

rank :: forall ds . KnownList ds => Word
rank = rank' (listRep @ds)
{-# INLINE rank #-}

rank' :: TypedList f ds -> Word
rank' (TypedList ds) = fromIntegral $ Prelude.length ds
{-# INLINE rank' #-}

-- TODO unsafe, remove
_reversed :: TypedList f d -> TypedList f d'
_reversed = unsafeCoerce . Prelude.reverse . unsafeCoerce 

-- | Type-indexed list
newtype TypedList (f :: Nat -> Type) (ds :: [Nat]) = TypedList [Any] 

-- | A list of evidence for constraints.
type EvidenceList (c :: Nat -> Constraint) ds = TypedList (Evidence' c) ds

-- | A list of type proxies.
type ProxyList ds = TypedList Proxy ds

-- | Get a constructible `ProxyList` from any other `TypedList`;
--   Pattern matching agains the result brings `KnownList` constraint
--   into the scope:
--
--   > case types ts of ProxyList -> ...
--
types :: TypedList f ds -> ProxyList ds
types (TypedList ds) = unsafeCoerce (Prelude.map (const Proxy) ds)
{-# INLINE types #-}

class Reflects s a | s -> a where
  -- | Recover a value inside a 'reify' context, given a proxy for its reified type.
  reflect :: proxy s -> a

-- | Known type lists.
--   Allows getting type information about list structure at runtime.
class KnownList ds where
  -- | Get type-level constructed list
  listRep :: ProxyList ds

instance KnownList ('[] :: [Nat]) where
  listRep = U

instance KnownList ds => KnownList (d :+ ds) where
  listRep = Proxy @d :* listRep @ds

--TODO is this useful?
--instance KnownList ds => Reflects ds (ProxyList ds) where reflect _ = listRep

newtype WithKnownList ds r = WithKnownList (KnownList ds => r)

--newtype WithSomeList r = WithSomeList (forall ds. KnownList ds => Proxy ds -> r)

-- | This function converts a user-supplied `listRep` function
--   to an instance of the `KnownList` typeclass at runtime.
reifyList' :: forall ds r
              . ProxyList ds
             -> (KnownList ds => r)
             -> r
reifyList' tl k = unsafeCoerce (WithKnownList k :: WithKnownList ds r) tl
{-# INLINE reifyList' #-}

-- | Pattern matching against this causes `KnownList` instance
--   come into scope.
--   Also it allows constructing a term-level list out of a constraint.
pattern ProxyList :: forall ds
                  . () => KnownList ds => ProxyList ds
pattern ProxyList <- (mkRTL -> E)
  where
    ProxyList = listRep @ds

-- | Pattern matching against this allows manipulating lists of constraints.
--   Useful when creating functions that change the shape of dimensions.
pattern EvList :: forall (c :: Nat -> Constraint) ds
                . () => (All c ds, KnownList ds) => EvidenceList c ds
pattern EvList <- (mkEVL -> E)
  where
    EvList = _evList (listRep @ds)

-- | Zero-length type list
pattern U :: forall (f :: Nat -> Type) ds
           . () => (ds ~ '[]) => TypedList f ds
pattern U <- (patTL @f @ds -> PatCNil)
  where
    U = unsafeCoerce []

-- | Zero-length type list; synonym to `U`.
pattern Empty :: forall (f :: Nat -> Type) ds
               . () => (ds ~ '[]) => TypedList f ds
pattern Empty = U

-- | Constructing a type-indexed list
pattern (:*) :: forall (f :: Nat -> Type) ds
              . ()
             => forall (y :: Nat) (ys :: [Nat])
              . (ds ~ (y ': ys)) => f y -> TypedList f ys -> TypedList f ds
pattern (:*) d ds = Cons d ds
infixr 5 :*

-- | Constructing a type-indexed list in the canonical way
pattern Cons :: forall (f :: Nat -> Type) (ds :: [Nat])
              . ()
             => forall (y :: Nat) (ys :: [Nat])
              . (ds ~ (y ': ys)) => f y -> TypedList f ys -> TypedList f ds
pattern Cons x ds <- (patTL @f @ds -> PatCons x ds)
  where
    Cons = Numeric.Tensile.Dimensions.Types.cons

-- | Constructing a type-indexed list from the other end
pattern Snoc :: forall (f :: Nat -> Type) (ds :: [Nat])
              . ()
             => forall (sy :: [Nat]) (y :: Nat)
              . (ds ~ (sy +: y)) => TypedList f sy -> f y -> TypedList f ds
pattern Snoc sx x <- (unsnocTL @f @ds -> PatSnoc sx x)
  where
    Snoc = Numeric.Tensile.Dimensions.Types.snoc

-- | Reverse a typed list
pattern Reverse :: forall (f :: Nat -> Type) (ds :: [Nat])
                 . ()
                => forall (sx :: [Nat])
                 . (ds ~ Reverse sx, sx ~ Reverse ds)
                => TypedList f sx -> TypedList f ds
pattern Reverse sx <- (unreverseTL @f @ds -> PatReverse sx)
  where
    Reverse = Numeric.Tensile.Dimensions.Types.reverse

-- Starting from GHC 8.2, compiler supports specifying lists of complete
-- pattern synonyms.
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE ProxyList #-}
{-# COMPLETE EvList #-}
{-# COMPLETE U, (:*) #-}
{-# COMPLETE U, Cons #-}
{-# COMPLETE U, Snoc #-}
{-# COMPLETE Empty, (:*) #-}
{-# COMPLETE Empty, Cons #-}
{-# COMPLETE Empty, Snoc #-}
{-# COMPLETE Reverse #-}
#endif

cons :: f x -> TypedList f ds -> TypedList f (x :+ ds)
cons x ds = TypedList (unsafeCoerce x : unsafeCoerce ds)
{-# INLINE cons #-}

snoc :: TypedList f ds -> f x -> TypedList f (ds +: x)
snoc ds x = TypedList (unsafeCoerce ds ++ [unsafeCoerce x])
{-# INLINE snoc #-}

reverse :: TypedList f ds -> TypedList f (Reverse ds)
reverse (TypedList sx) = unsafeCoerce (Prelude.reverse sx)
{-# INLINE reverse #-}

{-
take :: Dim n -> TypedList f ds -> TypedList f (Take n ds)
take d (TypedList ds) = unsafeCoerce (Prelude.take (_intD d) ds)
{-# INLINE take #-}

drop :: Dim n -> TypedList f ds -> TypedList f (Drop n ds)
drop d (TypedList ds) = unsafeCoerce (Prelude.drop (_intD d) ds)
{-# INLINE drop #-}
-}

head :: TypedList f ds -> f (Head ds)
head (TypedList ds) = unsafeCoerce (Prelude.head ds)
{-# INLINE head #-}

tail :: TypedList f ds -> TypedList f (Tail ds)
tail (TypedList ds) = unsafeCoerce (Prelude.tail ds)
{-# INLINE tail #-}

init :: TypedList f ds -> TypedList f (Init ds)
init (TypedList ds) = unsafeCoerce (Prelude.init ds)
{-# INLINE init #-}

last :: TypedList f ds -> f (Last ds)
last (TypedList ds) = unsafeCoerce (Prelude.last ds)
{-# INLINE last #-}

{-
length :: TypedList f ds -> Dim (Length ds)
length = order
{-# INLINE length #-}

splitAt :: Dim n
        -> TypedList f ds
        -> (TypedList f (Take n ds), TypedList f (Drop n ds))
splitAt d (TypedList ds) = unsafeCoerce (Prelude.splitAt (_intD d) ds)
{-# INLINE splitAt #-}
-}

concat :: TypedList f ds
       -> TypedList f ys
       -> TypedList f (ds ++ ys)
concat (TypedList ds) (TypedList ys) = unsafeCoerce (ds ++ ys)
{-# INLINE concat #-}

-- | Map a function over contents of a typed list
map :: (forall a . f a -> g a)
    -> TypedList f ds
    -> TypedList g ds
map k (TypedList ds) = unsafeCoerce (Prelude.map k' ds)
  where
    k' :: Any -> Any
    k' = unsafeCoerce . k . unsafeCoerce
{-# INLINE map #-}

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------


{-
_intD :: Dim n -> Int
_intD = (fromIntegral :: Word -> Int) . unsafeCoerce
-}

data PatReverse f ds
  = forall (sx :: [Nat]) . (ds ~ Reverse sx, sx ~ Reverse ds)
  => PatReverse (TypedList f sx)

unreverseTL :: forall f ds . TypedList f ds -> PatReverse f ds
unreverseTL (TypedList ds)
  = case (unsafeCoerce (E @(ds ~ ds, ds ~ ds))
           :: Evidence (ds ~ Reverse sx, sx ~ Reverse ds)
         ) of
      E -> PatReverse (unsafeCoerce (Prelude.reverse ds))
{-# INLINE unreverseTL #-}


mkRTL :: forall (ds :: [Nat])
       . ProxyList ds
      -> Evidence (KnownList ds)
mkRTL ds = reifyList' ds E
{-# INLINE mkRTL #-}


data PatSnoc f ds where
  PatSNil :: PatSnoc f '[]
  PatSnoc :: TypedList f ys -> f y -> PatSnoc f (ys +: y)

unsnocTL :: forall f ds . TypedList f ds -> PatSnoc f ds
unsnocTL (TypedList [])
  = case (unsafeCoerce (E @(ds ~ ds)) :: Evidence (ds ~ '[])) of
      E -> PatSNil
unsnocTL (TypedList (x:ds))
  = case (unsafeCoerce (E @(ds ~ ds)) :: Evidence (ds ~ (Init ds +: Last ds))) of
      E -> PatSnoc (unsafeCoerce sy) (unsafeCoerce y)
  where
    (sy, y) = unsnoc x ds
    unsnoc t []     = ([], t)
    unsnoc t (z:zs) = first (t:) (unsnoc z zs)
{-# INLINE unsnocTL #-}


data PatCons f ds where
  PatCNil :: PatCons f '[]
  PatCons :: f y -> TypedList f ys -> PatCons f (y ': ys)

patTL :: forall f ds . TypedList f ds -> PatCons f ds
patTL (TypedList [])
  = case (unsafeCoerce (E @(ds ~ ds)) :: Evidence (ds ~ '[])) of
      E -> PatCNil
patTL (TypedList (x : ds))
  = case (unsafeCoerce (E @(ds ~ ds)) :: Evidence (ds ~ (Head ds ': Tail ds))) of
      E -> PatCons (unsafeCoerce x) (unsafeCoerce ds)
{-# INLINE patTL #-}


mkEVL :: forall (c :: Nat -> Constraint) (ds :: [Nat])
       . EvidenceList c ds -> Evidence (All c ds, KnownList ds)
mkEVL U = E
mkEVL (E' :* evs) = case mkEVL evs of E -> E
#if __GLASGOW_HASKELL__ >= 802
#else
mkEVL _ = impossible
#endif

_evList :: forall (c :: Nat -> Constraint) (ds :: [Nat])
        . All c ds => ProxyList ds -> EvidenceList c ds
_evList U = U
_evList (_ :* ds) = case _evList ds of evs -> E' :* evs
#if __GLASGOW_HASKELL__ >= 802
#else
_evList _ = impossible
#endif
