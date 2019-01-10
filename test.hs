{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}

module Test where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Lens hiding (index, inside)
import Data.Data
import Data.Distributive
import Data.Foldable as Foldable
--import Data.Profunctor.Rep (Representable(..))
import Data.Proxy
import Data.Reflection as R
import Data.Tagged
import Data.Traversable (sequenceA)
import Data.Vector as V
import GHC.TypeLits
import GHC.Generics (Generic)
import GHC.Generics (Generic1)

{-
import Prelude hiding ((*), (+), (-), negate, subtract,zipWith)
import qualified Prelude
import qualified Data.Semiring as R 
-}

import qualified Control.Lens.Internal.Context as I

import Linear.Vector
-- import Linear.Covector
-- import Linear.Metric
-- import Linear.Conjugate

import Linear.V 

import Data.Foldable as Foldable

import Data.Coerce (coerce)
--import Data.Data.Lens
import Data.Functor.Rep --(Representable(..))



-- TensorLike t = (Traversable t, Additive t, Dim t, Dims t, Representable t, Foldable t, Finite t) 
-- also Elt a, Basis t

{-
-- a recursive typeclass for index flattening?
https://github.com/chessai/eigen/blob/89bb642f7fa0dbde0579b72c796b8706a3cf0c5b/src/Eigen/Internal.hsc#L81
class Cast (a :: Type) where
  type family C a = (result :: Type) | result -> a
  toC   :: a -> C a
  fromC :: C a -> a

instance Cast t => Cast (Dim n t) where fromC $! VS.unsafeIndex vals $! n * (fromC t + row

-}


-- TODO: add more base types. maybe replace TF OneOf with either of these approaches:
-- https://github.com/GU-CLASP/TypedFlow/blob/master/TypedFlow/Types.hs#L330-L443
-- http://hackage.haskell.org/package/hmatrix-0.19.0.0/docs/Numeric-LinearAlgebra.html#t:RealOf
-- | Class representing base elements of a tensor.
class (a ~ Float) => Elt a where

instance Elt Float

-- inductive typeclass for V a (V b (V c .... )) 
instance Elt a => Dims a where reflectDims _ = []
-- this would make no sense:
--instance Elt a => Dim a where reflectDim _ = 0

class Dims s where reflectDims :: proxy s -> [Int]


-- TODO: remove overlapping: https://mzabani.github.io/posts/2018-08-13.html
instance {-# Overlapping #-} (Dim (V n v), Dims v) => Dims (V n v) where
  reflectDims _ = reflectDim (Proxy :: Proxy (V n v)) : reflectDims (Proxy :: Proxy v)

instance {-# Overlapping #-} (Dim (T n v), Dims v) => Dims (T n v) where
  reflectDims _ = reflectDim (Proxy :: Proxy (T n v)) : reflectDims (Proxy :: Proxy v)

instance {-# Overlapping #-} (Dim (F n v), Dims v) => Dims (F n v) where
  reflectDims _ = reflectDim (Proxy :: Proxy (F n v)) : reflectDims (Proxy :: Proxy v)

dims :: forall a. (Dims a) => a -> [Int]
dims _ = reflectDims (Proxy :: Proxy a)



-- TODO: figure out biplate or use recursion schemes?
--instance (KnownNat n, Data a) => Plated (V n a) where plate = biplate


-- TODO: update linear version or fork.
vLens :: Int -> Lens' (V n a) a
vLens i = \f (V v) -> (\a -> V (v V.// [(i, a)])) <$> f (v V.! i)

instance ( 1 <= n) => Field1  (V n a) (V n a) a a where _1  = vLens  0
instance ( 2 <= n) => Field2  (V n a) (V n a) a a where _2  = vLens  1
instance ( 3 <= n) => Field3  (V n a) (V n a) a a where _3  = vLens  2
instance ( 4 <= n) => Field4  (V n a) (V n a) a a where _4  = vLens  3
instance ( 5 <= n) => Field5  (V n a) (V n a) a a where _5  = vLens  4
instance ( 6 <= n) => Field6  (V n a) (V n a) a a where _6  = vLens  5
instance ( 7 <= n) => Field7  (V n a) (V n a) a a where _7  = vLens  6
instance ( 8 <= n) => Field8  (V n a) (V n a) a a where _8  = vLens  7
instance ( 9 <= n) => Field9  (V n a) (V n a) a a where _9  = vLens  8


-- | nested functions:
-- TODO: change to other rep?
{-
HS.build ::
  (KnownNat n, KnownNat m) =>
  (HS.ℝ -> HS.ℝ -> HS.ℝ) -> HS.L m n

withDims 
  :: forall n m a. (Elem a, KnownNat n, KnownNat m) 
  => (Int -> Int -> VS.Vector (C a)) -> Matrix n m a
withDims f =
  let !r = natToInt @n
      !c = natToInt @m
  in Matrix $ Vec $ f r c

-- TODO: is this rep better for constructing tensors?
--newtype F' n t = F' { unF' :: Tagged n Int -> t } deriving (Functor,Applicative,Generic,Generic1)
type F' n t = Tagged n Int -> Vector t 
type F2' a b t = (KnownNat a, KnownNat b) => F' a (F' b t)
type F3' a b c t = (KnownNat a, KnownNat b, KnownNat c) => F' a (F' b (F' c t))

instance Num t => Num (Tagged n Int -> t) where
  (+) = liftA2 (+) 
  (-) = liftA2 (-) 
  (*) = liftA2 (*) 

-}



newtype F (n :: Nat) t = F { unF :: Int -> t } deriving (Functor,Applicative,Generic,Generic1)

instance Additive (F n)
instance Dim n => Dim (F n t) where reflectDim _ = reflectDim (Proxy :: Proxy n)

instance Num a => Num (V.Vector a) where
  as + bs = V.zipWith (+) as bs
  {-# INLINE (+) #-}
  as - bs = V.zipWith (-) as bs
  {-# INLINE (-) #-}
  as * bs = V.zipWith (*) as bs
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

instance Num a => Num (F n a) where
  F as + F bs = F $ liftA2 (+) as bs
  {-# INLINE (+) #-}
  F as - F bs = F $ liftA2 (-) as bs
  {-# INLINE (-) #-}
  F as * F bs = F $ liftA2 (*) as bs
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

type F2 a b t = F a (F b t)
type F3 a b c t = F a (F b (F c t))

f26 :: F2 2 6 Float 
f26 = pure 1

f265 :: F3 2 6 5 Float 
f265 = pure 1

-- | nested 'Tagged's:
newtype T (n :: Nat) t = T { unT :: Tagged n t } deriving (Eq,Ord,Show,Functor,Applicative,Foldable,Traversable,Generic,Generic1)

instance Dim n => Additive (T n)
instance Dim n => Dim (T n t) where reflectDim _ = reflectDim (Proxy :: Proxy n)

instance Dim n => Additive (Tagged n)



instance Num a => Num (T n a) where
  (+) = liftA2 (+) 
  (-) = liftA2 (-) 
  (*) = liftA2 (*) 
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}


type T2 a b t = T a (T b t)
type T3 a b c t = T a (T b (T c t))



-- TODO: sequence works but distribute doesn't for some reason
-- λ: t26 * (sequenceA $ t26 + t26)
-- Tagged (Tagged 2.0)

t26 :: T2 2 6 Float 
t26 = pure 1

-- test: dims t265 == [2,6,5]
t265 :: T3 2 6 5 Float 
t265 = pure 1

-- | nested 'Vector's:

type V2 a b t = (KnownNat a, KnownNat b) => V a (V b t) 
type V3 a b c t = (KnownNat a, KnownNat b, KnownNat c) => V a (V b (V c t)) 


{-
 -
 -
Representable f => 

instance {-# Overlapping #-} (Representable v, KnownNat n) => Representable (V n v) where
  reflectDims _ = reflectDim (Proxy :: Proxy (V n v)) : reflectDims (Proxy :: Proxy v)

type Rep ?  = Idxs ds -> a

instance (Representable f, Representable g) => Representable (Compose f g) where
  type Rep (Compose f g) = (Rep f, Rep g)
  index (Compose fg) (i,j) = index (index fg i) j
  tabulate = Compose . tabulate . fmap tabulate . curry


-}
-- TODO why isn't the applicative working?
b265 :: V3 2 6 5 Bool 
b265 = pure . pure . pure $ False

v5 :: V 5 Float
v5 = return 1

v52 :: V2 5 2 Float 
v52 = pure 1

v25 :: V2 2 5 Float 
v25 = pure 1

v265 :: V3 2 6 5 Float 
v265 = pure 1

v615 :: V3 6 1 5 Float 
v615 = pure 1

v22 :: V 2 (V 2 Int)
v22 = pure 1

v222 :: V 2 (V 2 (V 2 Int))
v222 = pure 5

v312 :: V 3 (V 1 (V 2 Int))
v312 = pure 2

v4312 :: V 4 (V 3 (V 1 (V 2 Int)))
v4312 = pure 2

-- test: dims v4321 == [4,3,2,1]
v4321 :: V 4 (V 3 (V 2 (V 1 Int)))
v4321 = pure 2


----Tensile.Operators API 
--

class Finite v where
  type Size v :: Nat -- this should allow kind k, for Reifies k Int
  toV :: v a -> V (Size v) a
  default toV :: Foldable v => v a -> V (Size v) a
  toV = V . V.fromList . Foldable.toList
  fromV :: V (Size v) a -> v a

instance Finite (V (n :: Nat)) where
  type Size (V n) = n
  toV = id
  fromV = id

-- TODO: remove
add :: (Additive f, Num a) => f a -> f a -> f a
add = (^+^)

sub :: (Additive f, Num a) => f a -> f a -> f a
sub = (^-^)

-- test: dim (scalar 1.7 :: V 1 Float) == 1
scalar :: (Additive f, Finite f, Elt a) => a -> f a
scalar a = fromV (V $ pure a)

-- (Tensor f, Ring a) 
(.+.) :: (Additive f, Num a) => f a -> f a -> f a 
(.+.) = add 

(.-.) :: (Additive f, Num a) => f a -> f a -> f a 
(.-.) = sub 

-- | Right scalar product
(.*) :: (Functor f, Num a) => f a -> a -> f a
f .* a = fmap (* a) f

-- | Left scalar product
(*.) :: (Functor f, Num a) => a -> f a -> f a
a *. f = fmap (* a) f

-- | Right vector product
(.*!) :: (Functor m, Foldable r, Additive r, Num a) => m (r a) -> r a -> m a
m .*! v = fmap (\r -> Foldable.sum $ liftI2 (*) r v) m

-- (!*) :: (Metric r, Additive f, Num a) => r a -> r (f a) -> f a
-- f !* g = dot f <$> distribute g
--
-- | Left vector product
(!*.) :: (Foldable t, Additive f, Additive t, Num a) => t a -> t (f a) -> f a
f !*. g = sumV $ liftI2 (*.) f g

-- | Matrix product. 
(.*.) :: (Functor m, Foldable t, Additive t, Additive n, Num a) => m (t a) -> t (n a) -> m (n a)
f .*. g = fmap (\f' -> Foldable.foldl' add zero $ liftI2 (*.) f' g) f

{-
infixl 6 :+:, :-:
infixl 7 :*:,.*:,:*.,*:,:*




sumV :: (Foldable f, Additive v, Num a) => f (v a) -> v a
sumV = Foldable.foldl' add zero

dot :: (Foldable f, Num a) => f a -> f a -> a
dot x y = Foldable.sum $ liftI2 R.(*) x y

-- | Product of a matrix of weights with a batched vector .
weight
  :: (Functor m, Foldable t, Additive t, Applicative t, Additive n, Num a, Traversable n) 
  => n (t a) -> m (t a) -> m (n a)
weight m v = v .*. (sequenceA m) -- use distribute here instead?

--transpose :: (Distributive g, Functor f) => f (g a) -> g (f a)
--transpose = distribute

-}

-- | A generalization of 'Control.Lens.inside' that works over any corepresentable 'Functor'.
-- Useful for performing operations along different axes. See https://medium.com/@aerinykim/tensorflow-101-what-does-it-mean-to-reduce-axis-9f39e5c6dea2
-- for a tutorial on tensor axes.

-- λ: :t v4312 ^. (_1)
-- v4312 ^. (_1) :: V 3 (V 1 (V 2 Int))
-- λ: :t v4312 ^. (_1 . _1)
-- v4312 ^. (_1 . _1) :: V 1 (V 2 Int)
-- λ: :t v4312 ^. (_1 . _1 . _1)
-- v4312 ^. (_1 . _1 . _1) :: V 2 Int
-- λ: :t v4312 ^. (_1 . _1 . _1 ._1)
-- v4312 ^. (_1 . _1 . _1 ._1) :: Int
-- 
-- λ: :t v4312 ^. inside (_1 )
-- v4312 ^. inside (_1 ) :: V 4 (V 1 (V 2 Int))
-- λ: :t v4312 ^. inside (_1 . _1)
-- v4312 ^. inside (_1 . _1) :: V 4 (V 2 Int)
-- λ: :t v4312 ^. inside (_1 . _1 . _1)
-- v4312 ^. inside (_1 . _1 . _1) :: V 4 Int
-- 
-- λ: :t v4312 ^. (inside _1)
-- v4312 ^. (inside _1) :: V 4 (V 1 (V 2 Int))
-- λ: :t v4312 ^. (inside . inside $ _1)
-- v4312 ^. (inside . inside $ _1) :: V 4 (V 3 (V 2 Int))
-- λ: :t v4312 ^. (inside . inside . inside $ _1)
-- v4312 ^. (inside . inside . inside $ _1) :: V 4 (V 3 (V 1 Int))

inside
  :: Functor f
  => Representable g 
  => LensLike (Context a b) s t a b
  -> (g a -> f (g b)) -> g s -> f (g t)
inside l = \afb s -> sbt s <$> afb (sa s) 
  where
    sa = liftR $ \s -> I.ipos (accessors l s)

    sbt = liftR2 $ \s b -> I.ipeek b (accessors l s)

    -- Isomorphic to s -> (a, (b -> t)) 
    accessors l = l (Context id)

    liftR = fmapRep



-- TODO: push (V 1) constructor down n levels to add an extra dimension at axis (@n@) of size 1.
--
-- expandDim :: forall n s t. (KnownLen s, KnownPeano n) => Tensor s t -> Tensor (Take n s ++ (1 ': Drop n s)) t
-- expandDim (T x) = (T (funcall "tf.expand_dims" [x, named "axis" (integer (listLen @s - peanoInt @n))]))

--reduceSum0 :: ∀ s' n t. KnownLen s' => Tensor (n ': s') t -> Tensor s' t
reduceSum :: (Foldable f, Additive v, Num a) => f (v a) -> v a
reduceSum = sumV

-- dot prod along arbitrary axis
--
-- λ: :t v265 `dot` v265
-- v265 `dot` v265 :: V 6 (V 5 Float)
-- 
-- λ: :t liftI2 dot v265 v265
-- liftI2 dot v265 v265 :: V 2 (V 5 Float)
-- 
-- | For two tensors of the same dimension, the Hadamard product is a tensor of the same dimension as the operands, with elements given by pointwise multiplication.

--hadamard :: (Num a, Additive f) => f a -> f a -> f a
--hadamard = liftI2 (Prelude.(*))

{-
-- https://en.wikipedia.org/wiki/Kronecker_product
If A is an m × n matrix and B is a p × q matrix, then the Kronecker product A ⊗ B is the mp × nq block matrix formed by multiplying every scalar value in m by the matrix m.

Transposition and conjugate transposition are distributive over the Kronecker product:

transpose (m ⊗ n) == (transpose m) ⊗ (transpose n)
adjoint (m ⊗ n) == (adjoint m) ⊗ (adjoint n)

TODO: replace w/ version w/ fewer restrictions on a. 
need to recurse to leaves and then build up matrices. zylomorph?
-}
kronecker :: (Functor f, Functor g, Num a) => f a -> g a -> f (g a)
kronecker = outer


-- $setup
-- >>> import Data.Complex
-- >>> import Data.IntMap
-- >>> import Debug.SimpleReflect.Vars
-- >>> import Linear.V





-- need Linear.Conjugate for this
-- | Hermitian conjugate or conjugate transpose
--
-- >>> adjoint (V2 (V2 (1 :+ 2) (3 :+ 4)) (V2 (5 :+ 6) (7 :+ 8)))
-- V2 (V2 (1.0 :+ (-2.0)) (5.0 :+ (-6.0))) (V2 (3.0 :+ (-4.0)) (7.0 :+ (-8.0)))
-- adjoint :: (Functor m, Distributive n, Conjugate a) => m (n a) -> n (m a)
-- adjoint = collect (fmap conjugate)
-- {-# INLINE adjoint #-}

