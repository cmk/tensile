{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE LambdaCase           #-}

{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE UndecidableInstances   #-} 
{-# LANGUAGE ViewPatterns           #-}
module Numeric.Tensile.Dimensions.Dim where

import Data.Proxy
import Numeric.Type.Evidence
import Unsafe.Coerce (unsafeCoerce)

import Data.Kind (Type)
import           GHC.Exts           (Constraint, Proxy#, proxy#, unsafeCoerce#)
--import           GHC.Exts           (Constraint)
--import GHC.TypeLits
import           Data.Type.Bool
import           Data.Type.Equality
import           GHC.TypeLits       as TL

--import Numeric.Tensile.Dimensions.Dim.Class
import Numeric.Tensile.Dimensions.Types (Reifies(..), type (<))

--TODO hide DimSing constructor

plusDim :: Dim n -> Dim m -> Dim (n + m)
plusDim (DimSing a) (DimSing b) = unsafeCoerce# (a + b)
{-# INLINE plusDim #-}

minusDim :: m < n => Dim n -> Dim m -> Dim (n - m)
minusDim (DimSing a) (DimSing b) = unsafeCoerce# (a - b)
{-# INLINE minusDim #-}

minusDimM :: Dim n -> Dim m -> Maybe (Dim (n - m))
minusDimM (DimSing a) (DimSing b)
  | a > b    = Just (unsafeCoerce# (a - b))
  | otherwise = Nothing
{-# INLINE minusDimM #-}

timesDim :: Dim n -> Dim m -> Dim ((TL.*) n m)
timesDim (DimSing a) (DimSing b) = unsafeCoerce# (a * b)
{-# INLINE timesDim #-}

powerDim :: Dim n -> Dim m -> Dim ((TL.^) n m)
powerDim (DimSing a) (DimSing b) = unsafeCoerce# (a ^ b)
{-# INLINE powerDim #-}


--type Dims (d :: [Nat]) = DS.Dims d
-- type Dims (ds :: [Nat]) = TypedList Dim ds
--

--type Dim (d :: Nat) = D.Dim d

-- | Singleton type to store type-level dimension value.
-- Dimensions are restricted to strictly positive naturals.
--data Dim :: Nat -> Type where DimSing :: 1 <= d => Word -> Dim d

newtype Dim (d :: Nat) = DimSing Word

-- | Similar to `natVal` from `GHC.TypeLits`, but returns `Word`.
dimVal' :: Dim d -> Word
dimVal' = unsafeCoerce -- \case DimSing d -> d
--unsafeCoerce#
{-# INLINE dimVal' #-}

-- | Similar to `natVal` from `GHC.TypeLits`, but returns `Word`.
dimVal :: forall d. KnownDim d => Word
dimVal = dimVal' (dim @d)
{-# INLINE dimVal #-}

--instance Show SomeDim where show (SomeDim d) = show d
instance Show SomeDim where show _ = "SomeDim"

show' (SomeDim d) = show d


data SomeDim = forall d. KnownDim d => SomeDim (Dim d)

someDim :: Word -> Maybe SomeDim
someDim w = reifyDim w SomeDim

{-
data SomeNat    = forall n. KnownNat n    => SomeNat    (Proxy n)

-- | Convert an integer into an unknown type-level natural.
--
-- @since 4.10.0.0
someNatVal :: Natural -> SomeNat
someNatVal n = withSNat SomeNat (SNat n) Proxy

someNatVal :: Natural -> SomeNat
someNatVal n = withSNat SomeNat (SNat n) Proxy

--private
newtype SNat    (n :: Nat)    = SNat    Natural

data WrapN a b = WrapN (KnownNat    a => Proxy a -> b)

-- See Note [magicDictId magic] in "basicType/MkId.hs"
withSNat :: (KnownNat a => Proxy a -> b)
         -> SNat a      -> Proxy a -> b
withSNat f x y = magicDict (WrapN f) x y
-}


instance KnownDim d => Reifies d (Dim d) where
  reflect _ = dim

newtype WithKnownDim d r = WithKnownDim (KnownDim d => r)

newtype WithSomeDim r = WithSomeDim (forall d. KnownDim d => Proxy d -> r)

reifyKnownDim :: forall d r . Dim d -> (KnownDim d => r) -> r
reifyKnownDim d k = unsafeCoerce (WithKnownDim k :: WithKnownDim d r) d

unsafeReifyDim :: forall r. Word -> (forall d. KnownDim d => Proxy d -> r) -> r
unsafeReifyDim w k = unsafeCoerce (WithSomeDim k :: WithSomeDim r) w Proxy

reifyDim :: forall r. Word -> (forall d. KnownDim d => Dim d -> r) -> Maybe r
reifyDim w k = if w == 0 then Nothing else Just r
  where r = unsafeReifyDim w $ \p -> k (reflect p) 

dimEv :: Dim d -> Evidence (KnownDim d)
dimEv d = reifyKnownDim d E
{-# INLINE dimEv #-}

-- | We either get evidence that this function
--   was instantiated with the same type-level numbers, or Nothing.
--
--   Note, this function works on @Nat@-indexed dimensions only,
--   because @Dim (XN x)@ does not have runtime evidence to infer @x@
--   and `KnownDim x` does not imply `KnownDim (XN x)`.
sameDim' :: forall (x :: Nat) (y :: Nat)
         . Dim x -> Dim y -> Maybe (Evidence (x ~ y))
sameDim' (DimSing a) (DimSing b)
  | a == b    = Just (unsafeCoerce# (E @(x ~ x)))
  | otherwise = Nothing
{-# INLINE sameDim' #-}

-- | We either get evidence that this function
--   was instantiated with the same type-level numbers, or Nothing.
sameDim :: forall (x :: Nat) (y :: Nat) p q
          . (KnownDim x, KnownDim y)
         => p x -> q y -> Maybe (Evidence (x ~ y))
sameDim _ _ = sameDim' (dim @x) (dim @y)
{-# INLINE sameDim #-}

-- | Ordering of dimension values.
compareDim' :: Dim a -> Dim b -> Ordering
compareDim' da db = dimVal' da `compare` dimVal' db
{-# INLINE compareDim' #-}


-- | Ordering of dimension values.
compareDim :: forall a b. (KnownDim a, KnownDim b) => Ordering
compareDim = compareDim' (dim @a)  (dim @b)
{-# INLINE compareDim #-}

-- | Independently of the kind of type-level number,
--   construct an instance of `KnownDim` from it.
--
--   Match against this pattern to bring `KnownDim` instance into scope.
pattern Dim :: forall d. KnownDim d => Dim d
pattern Dim <- (dimEv -> E)
  where
    Dim = dim @d
-- Starting from GHC 8.2, compiler supports specifying lists of complete
-- pattern synonyms.
#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE Dim #-}
#endif



instance Eq (Dim d) where
    _ == _ = True
    {-# INLINE (==) #-}

instance Show (Dim d) where
    showsPrec p = showsPrec p . dimVal'
    {-# INLINE showsPrec #-}

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
