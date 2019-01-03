



Some NN building blocks are naturally higher-order. Taking an example (and simplifying) a recurrent neural network turns a tensor function into a function between lists (vectorslists) of tensors.
Functional programming is ideally suited to program complicated applications from building blocks.
Example: an “Attention-model” is a thing where every step in a RNN adds a computation which depends on an external input. We can compose usual RNN cells with attention models in several ways. The state of the art is to reprogram all combinations by hand.

Typed APIs.
Types can be used to check the tensor dimensions. Types catch a lof of errors, but they can also be used to guide the programming.

Types are pretty much a necessity in the presence of HO functions.

TypedFlow is typically much closer to mathematical notation than python. Programs are short to write and easier to read. Standard building blocks can be swapped for custom versions quite easily.
Examples

rnn stacking using “residual connections” instead of just stacking.
make it easy to share parameters between different components (example: if we do a style translation we may want to share the embedding layers between encoder and decoders parts)
Long game: integrate cutting edge ideas as they arrive with moderate effort.

-----------------------------------------------------------------
tensile - a flexible high-level n.n. API (think FP Keras) 
-----------------------------------------------------------------
multiple backends 
  - tensile-hmatrix
  - tensile-eigen 
  - tensile-accelerate    
    - https://github.com/AccelerateHS/accelerate
    - https://github.com/ekmett/linear-accelerate/
  - tensile-tensorflow


TOGROK

https://github.com/GU-CLASP/TypedFlow/blob/master/TypedFlow/Haskell.hs#L99
  - grok column: http://hackage.haskell.org/package/linear-1.20.8/docs/src/Linear.Matrix.html#column
  'column' is a generalization of 'Control.Lens.inside' to work over any corepresentable 'Functor'
  generalize to n-d slice

  - grok Distributive / Representable
  - grok Conjoined, Indexable, Indexing, Index, Ixed 
http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Indexed.html 
http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Internal-Indexed.html#t:Indexable
  - grok biplate / Data-Data-Lens


-- TODO: 

Numeric.Tensile
Numeric.Tensile.Tensor


  - give Bool a num instance? going to fail a - a = 0
  - use http://hackage.haskell.org/package/semirings-0.2.1.1/docs/Data-Semiring.html

-- plus, zero, times, one

import Prelude hiding ((*), (+), (-), negate, subtract,zipWith)
import qualified Prelude
import qualified  Data.Semiring as S



class Dims v => Finite v where
  type Shape v :: [Nat] ??

  type Size v :: Nat -- this should allow kind k, for Reifies k Int
  toV :: v a -> V (Size v) a
  default toV :: Foldable v => v a -> V (Size v) a

  toV = V . V.fromList . Foldable.toList
  fromV :: V (Size v) a -> v a

  -- need to get shape
  fill :: V (Size v) a -> v a


instance (Tensor (t s a), Ring a) => Ring (t s a) where

  add :: Num a => t s a -> t s a -> t s a

  neg :: Num a => t s a -> t s a

  sub :: Num a => t s a -> t s a -> t s a
  sub x y = x `add` neg y

  mul :: Num a => t s a -> t s a


class Tensor f where 

  -- Tensor (Kronecker) product 
  -- transpose (m ⊗ n) == (transpose m) ⊗ (transpose n)
  -- adjoint (m ⊗ n) == (adjoint m) ⊗ (adjoint n)

  (:*:) :: t s a -> t s' a -> t (s++s') a 

  

  transpose :: Rank 1 f, Tensor g => f * g -> g * f

  -- f has rank 1
  transpose :: Tensor g => f (g a) -> g (f a)



  -- https://github.com/ekmett/linear/blob/master/src/Linear/Trace.hs
  trace :: f (g (h a)) -> h a

  -- contract 

  -- a (1,1) tensor action
  (.*.) :: (Tensor g, Tensor t, Num a) => f (t a) -> t (g a) -> f (g a)

  -- a (1,
  (.*) :: (Tensor g, Num a) => f a -> f (g a) -> f a

  (*.) :: (Tensor g, Num a) => f (g a) -> f a -> f a

  (..*) :: Num a => a -> f a -> f a

  (*..) :: Num a => f a -> a -> f a


class Tensile (t s a) where

  constant :: s -> a -> t s a 

  -- s1234 have optic-like relationship, 
  -- easy to impl w/ Fuctor/Foldable w/ Index
  tmap :: (t s1 a -> t s2 a) -> t s3 a -> t s4 a

  tmap :: (t n a -> t s2 a) -> t s3 a -> t s4 a

  -- transpose :: f (g a) -> g (f a)
  shuffle :: (s -> s') -> t s a -> t s' a

  unstack :: t (n:s) a -> V n (t s a)
  unstack :: f n (g s a) -> V n (g s a)

  stack :: V n (t s a) -> t (n:s) a


-- mapT' :: forall s t r u n. KnownLen r => KnownLen s => KnownNat n => (T s t -> T r u) ->  T (n ': s) t -> Gen (T (n ': r) u)
-- mapT' f t = do
--   xs <- unstack t
--   return (stack (fmap f xs))

-- | Map a function along the first dimension of a tensor
mapT :: forall s t r u n. KnownTyp u => KnownLen r => KnownLen s => (T s t -> T r u) ->  T (n ': s) t -> Gen (T (n ': r) u)
mapT f x = do
  x' <- mapTN @n f (transposeN @s @n x)
  return (transposeN' @r x')

zipWithT :: forall (s :: [Nat]) (t :: Typ) (s1 :: [Nat]) (t1 :: Typ) (s2 :: Shape) (n :: Nat) (t2 :: Typ).
            KnownNat n => (KnownLen s, KnownLen s2, KnownLen s1) => KnownTyp t2 =>
                  (T s t -> T s1 t1 -> T s2 t2)
                  -> Tensor (n ': s) t
                  -> Tensor (n ': s1) t1
                  -> Gen (Tensor (n ': s2) t2)
zipWithT f x y = do
  -- xs <- unstack x
  -- ys <- unstack y
  -- return (stack (f <$> xs <*> ys))
  x' <- zipWithTN @n f (transposeN @s @n x) (transposeN @s1 @n y)
  return (transposeN' @s2 x')


# If x is complex, setting conjugate=True gives the conjugate transpose

# 'perm' is more useful for n-dimensional tensors, for n > 2
x = tf.constant([[[ 1,  2,  3],
                  [ 4,  5,  6]],
                 [[ 7,  8,  9],
                  [10, 11, 12]]])

# Take the transpose of the matrices in dimension-0
# (this common operation has a shorthand `linalg.transpose`)
tf.transpose(x, perm=[0, 2, 1])  # [[[1,  4],
                                 #   [2,  5],
                                 #   [3,  6]],
                                 #  [[7, 10],
                                 #   [8, 11],
                                 #   [9, 12]]]




-- keep this API relatively pure. Euclidean f?

instance (Eq f, Dim f, Traversable f, Applicative f) => Tensor f where 

  -- need to use pure 0 rather than [] due to dimensional eq constraints

  constant = pure

  matmul f g = fmap (\f' -> Foldable.foldl' add zero $ liftI2 (!*) f' g) f

  add = liftA2 R.plus

  neg = fmap R.negate

  -- use distribute?
  transpose = sequenceA

  -- need to restrict arg to what TF can use
  liftU2 :: (a -> a -> a) -> f a -> f a -> f a
  liftU2 = liftA2

  liftI2 :: (a -> a -> a) -> f a -> f a -> f a
  liftI2 = liftU2

(f a -> g a) -> f a

negated :: (Functor f, Num a) => f a -> f a
negated = fmap R.negate

zero :: Num a => f a 
zero = constant R.zero


(.*.) :: (Tensor s, Tensor s', Tensor t, Num a) 
      => s (t a) -> t (s' a) -> s (s' a)

f .*. g = matmul f g


scalar :: (Additive f, Finite f, Elt a) => a -> f a
scalar a = fromV (V $ pure a)


    fromInteger = scalar . fromInteger
    signum = CoreOps.sign
    negate = CoreOps.neg

oneHot

laws:
  negate a = zero - a
  sub a b  = a + neg b 
  sub a b  = negate a + b
r\cdot (x+y)=r\cdot x+r\cdot y
{\displaystyle (r+s)\cdot x=r\cdot x+s\cdot x} (r+s)\cdot x=r\cdot x+s\cdot x
{\displaystyle (rs)\cdot x=r\cdot (s\cdot x)} (rs)\cdot x=r\cdot (s\cdot x)
{\displaystyle 1_{R}\cdot x=x.} 1_{R}\cdot x=x.



-- | The identity matrix (not necessarily square)
identity :: forall n m a. (Elem a, KnownNat n, KnownNat m) => Matrix n m a
identity =
  Internal.performIO $ do
     m :: M.IOMatrix n m a <- M.new
     Internal.call $ M.unsafeWith m Internal.identity
     unsafeFreeze m


mean => Tensor v'1 t	
input

-> Tensor v'2 tidx	
reduction_indices

-> Tensor Build t


data Row (r :: Nat) = Row
-- | Like 'Proxy', but specialised to 'Nat'.
data Col (c :: Nat) = Col

coeff :: (Tensor f, Num a) => [Int] -> a
(!) = coeff

coeff _ _ m@(Matrix (Vec vals)) =
  let !row  = natToInt @r
      !col  = natToInt @c
  in fromC $! VS.unsafeIndex vals $! col * rows m + row



class (LeftModule Integer r, RightModule Integer r, Monoidal r) => Group r where
  (-)      :: r -> r -> r
  negate   :: r -> r
  subtract :: r -> r -> r
  times    :: Integral n => n -> r -> r
  times y0 x0 = case compare y0 0 of
    LT -> f (negate x0) (Prelude.negate y0)
    EQ -> zero
    GT -> f x0 y0
    where
      f x y 
        | even y = f (x + x) (y `quot` 2)
        | y == 1 = x
        | otherwise = g (x + x) ((y Prelude.- 1) `quot` 2) x
      g x y z 
        | even y = g (x + x) (y `quot` 2) z
        | y == 1 = x + z
        | otherwise = g (x + x) ((y Prelude.- 1) `quot` 2) (x + z)







- build API side on top of linear instead of hmatrix. make tensor module for linear.

  - Dim v (Reifies dimension), Finite v
  - Finite v, Traversable v => Basis v 
    - basis, basisFor, scaled, unit
    - (Int -> Lens' (v a) a) (see vLens) 


  - SizedFunctor & SizedFunctorWithIndex? to capture phantom dim size

  - are distributive / transpose, applicative / ^++^ enough to avoid too much code? answer two questions: how to slice w/ V n in linear, and how to encode TF manipulations w/ the (->) x instance of Additive.

    - map_fn for liftU2? https://www.tensorflow.org/api_docs/python/tf/map_fn
      https://github.com/tensorflow/tensorflow/blob/r1.12/tensorflow/python/ops/functional_ops.py

  - how to represent TF actions? 
    - https://github.com/GU-CLASP/TypedFlow/blob/master/TypedFlow/Types.hs#L619
    - use Free applicative w/ Build interpreter?
    - generalize to recursively defined T / use MonadFix?

  - do we want newtype wrappers for T1,T2 etc? use Index/Idxed for all V?

  - connect Reifies from linear to Reifies s W from backprop (instance Reifies s Shape)

  - add module generalizing Static.Backprop to tensors

- build TF side on top of typed-flow instead of tf-haskell
  - use ST or Modify API for TF Refs instead? see TF.assign
 
- use Indexed/Ixed and bases for slicing (e.g. basisFor (zero :: V 5 (V 2 Float)))
newtype E t = E { el :: forall x. Lens' (t x) x }



- why to tensors not have dim info? is it in protos?
- start with Output, Types, FFI, Build modules
  


-- later replace TensorType with Elt or Cast? need sm like `Ptr (C a)`?
class TensorType a where
    tensorType :: a -> DataType
    tensorRefType :: a -> DataType
    tensorVal :: Lens' TensorProto [a]

class Cast (a :: Type) where
  type family C a = (result :: Type) | result -> a
  toC   :: a -> C a
  fromC :: C a -> a

-- | `Elem` is a closed typeclass that encompasses the properties
--   eigen expects its values to possess, and simplifies the external
--   API quite a bit.
class (Num a, Cast a, Storable a, Storable (C a), Code (C a)) => Elt a


eigen    | hmatrix   | linear      | typed flow
------------------------------------
dims     | dim       | dim / Dim   | 
         | Sized     | Finite      |
         | Indexable | Index, Ixed |
coeff    | atIndex   | -           |
withDims | build     | toV / toT   |


Numeric.LinearAlgebra.Data konst :: Konst e d c => e -> d -> c e
Numeric.LinearAlgebra.Static konst :: Sized t s d => t -> s
Numeric.LinearAlgebra.Static.Backprop konst :: forall t s d q . (Reifies q W, Sized t s d, Container d t, Num s) => BVar q t -> BVar q s
Numeric.LinearAlgebra.Data class Konst e d c | d -> c, c -> d


class Dim n where
  reflectDim :: p n -> Int

toV :: Foldable v => v a -> V (Size v) a

dims 
  :: forall n m a. (Elem a, KnownNat n, KnownNat m) 
  => Matrix n m a -> (Int, Int)
dims _ = (natToInt @n, natToInt @m)


dim :: forall n . KnownNat n => R n
dim = v
  where
    v = mkR (scalar (fromIntegral $ size v))

dim :: forall n a. Dim n => V n a -> Int
dim _ = reflectDim (Proxy :: Proxy n)


-- tensorflow

data TensorData = TensorData
    { tensorDataDimensions :: [Int64]
    , tensorDataType       :: !DataType
    , tensorDataBytes      :: !(S.Vector Word8)
    }
  deriving (Show, Eq)

-- TensorFlow.Tensor
data Feed = Feed Output FFI.TensorData

-- TODO: remove and replace w/ 
-- 'S.Vector' is the most efficient to encode/decode for most element types.
class TensorType a => TensorDataType s a where
    -- | Decode the bytes of a 'TensorData' into an 's'.
    decodeTensorData :: TensorData a -> s a
    -- | Encode an 's' into a 'TensorData'.
    --
    -- The values should be in row major order, e.g.,
    --
    --   element 0:   index (0, ..., 0)
    --   element 1:   index (0, ..., 1)
    --   ...
    encodeTensorData :: Shape -> s a -> TensorData a


-- typed-flow
newtype V (n::Nat) a = V [a]
  deriving (Functor, Foldable, Traversable)

instance KnownNat n => Applicative (V n) where
  pure = V . replicate (fromIntegral (natVal (Proxy @n)))
  V fs <*> V xs = V (zipWith ($) fs xs)

-- compare w/ linear:
instance Dim n => Applicative (V n) where
  pure = V . V.replicate (reflectDim (Proxy :: Proxy n))
  V as <*> V bs = V (V.zipWith id as bs)

data T (shape :: Shape) (t :: Typ) = T {fromTensor :: UntypedExpression}
https://github.com/GU-CLASP/TypedFlow/blob/master/TypedFlow/Types.hs#L619

-- | Concatenate @n@ tensors along the first dimension
stack0 :: ∀ s (n::Nat) t. (KnownLen s) => V n (T s t) -> Tensor (n ': s) t
stack0 (V xs) = T (funcall "tf.stack" [list [x | T x <- xs], text "axis=" <> integer (listLen @ s)])

-- | Concatenate @n@ tensors along the first dimension
stack1 :: ∀ s (n::Nat) m t. (KnownLen s) => V n (T (m ': s) t) -> Tensor (m ': n ': s) t
stack1 (V xs) = T (funcall "tf.stack" [list [x | T x <- xs], text "axis=" <> integer (listLen @ s)])

-- | Concatenate @n@ tensors along the last dimension
stackN :: ∀ s (n::Nat) t. V n (T s t) -> Tensor (s ++ '[n]) t
stackN (V xs) = T (funcall "tf.stack" [list [x | T x <- xs], text "axis=0"])


-- linear
-- -XKindSignatures, -XDataKinds


-- https://github.com/ekmett/linear/blob/master/src/Linear/V.hs#L131

instance KnownNat n => Dims (n :: Nat) where reflectDims _ = pure . fromInteger . natVal 
instance KnownNat n => Dims n where reflectDims _ = pure . fromInteger $ natVal (Proxy :: Proxy n)
 


reflectDim = fromInteger . natVal

instance (KnownNat n, Elt a) => Dims (V n a) where 
  reflectDims _ = pure . fromInteger $ natVal (Proxy :: Proxy n)

instance (Dim n, Elt a) => Dims (V n a) where 
  reflectDims _ = pure . fromInteger $ natVal @n

instance (Dim n) => Dims n where 
  reflectDims :: forall k (s :: k) (p :: k -> *). Dim s => p s -> [Int]
  reflectDims _ = pure $ reflectDim (Proxy :: Proxy n)



instance Reifies s Int => Dims (ReifiedDim s) where reflectDims = retagDim reflect



-- class (Num a, Cast a, Storable a, Storable (C a), Code (C a)) => Elt a



reifyDims :: Int -> (forall (s :: *). Dims s => Proxy s -> r) -> r
reifyDims i f = R.reify i (go f) where
  go :: (Proxy (ReifiedDims n) -> a) -> proxy n -> a
  go g _ = g Proxy

reifyVector :: forall a r. Vector a -> (forall (n :: *). Dim n => V n a -> r) -> r
reifyVector v f = reifyDim (V.length v) $ \(Proxy :: Proxy n) -> f (V v :: V n a)

dim :: forall n a. Dim n => V n a -> Int
dim _ = reflectDim (Proxy :: Proxy n)

class Nested t where
  type Dims (t :: * -> *) :: Shape -- this should allow kind k, for Reifies k Int
  toT :: t a -> T (Dims t) a
  default toT :: Foldable t => t a -> T (Shape t) a
  toT = T . T.fromList . Foldable.toList
  fromT :: T (Shape t) a -> t a

newtype E t = E { el :: forall x. Lens' (t x) x }

instance R1 T2 where
  _x :: Lens' (t a) a
  _x = _T2 . _1



--import GHC.Generics, -XDeriveGeneric, DeriveFunctor, GeneralizedNewtypeDeriving,DeriveDataTypeable
newtype T2 a b t = T2 { _unT2 :: V a (V b t) } deriving (Eq,Ord,Show,Read,Functor,Data,Generic,Generic1)
makePrisms ''T2

  
class R3 t => R4 t where
  -- |
  -- >>> V4 1 2 3 4 ^._w
  -- 4
  _w :: Lens' (t a) a
  _xyzw :: Lens' (t a) (V4 a)


λ: :t v265 !*! v615 -- lower shape (5) is preserved
v265 !*! v615 :: V 2 (V 1 (V 5 Float))



-- flat vector
newtype T s a = T { toVector :: V.Vector a } 

type T1  a t = Dim a (Vector t) 
type T1' a t = Dim a Output 

type T2 a b t = Dim a (Dim b (V.Vector t)) 
type T2' a b t = Dim a (Dim b Output) 



----API copied from typed-flow





as !+! bs = 
-- backprop / hmatrix-backprop

data BVar s a = BV { _bvRef :: !(BRef s)
                   , _bvVal :: !a
                   }

-- | @since 0.1.5.1
deriving instance Typeable (BVar s a)

data BRef (s :: Type) = BRInp !Int
                      | BRIx !Int
                      | BRC
  deriving (Generic, Show)

instance NFData (BRef s)


-- hmatrix 
(><) :: Storable a => Int -> Int -> [a] -> Matrix a
-- hmatrix Internal.Static

newtype Dim (n :: Nat) t = Dim t deriving Show (Show, Generic)

-- | generic indexing function
--
-- >>> vector [1,2,3] `atIndex` 1
-- 2.0
--
-- >>> matrix 3 [0..8] `atIndex` (2,0)
-- 6.0
--
atIndex :: Container c e => c e -> IndexOf c -> e

HS.withMatrix ::
      Matrix HS.ℝ
  -> (forall (m :: Nat) (n :: Nat). (KnownNat m, KnownNat n) => HS.L m n -> z)
  ->  z

HS.build ::
  (GHC.TypeLits.KnownNat n, GHC.TypeLits.KnownNat m) =>
  (HS.ℝ -> HS.ℝ -> HS.ℝ) -> HS.L m n

c :: (KnownNat m, KnownNat n) => L m n
c = build (\r c -> r**2 - c/2)




-- eigen API
newtype Matrix :: Nat -> Nat -> Type -> Type where
  Matrix :: Vec (n * m) a -> Matrix n m a

-- | Used internally to track the size and corresponding C type of the matrix.
newtype Vec :: Nat -> Type -> Type where
  Vec :: VS.Vector (C a) -> Vec n a

-- | Like 'Proxy', but specialised to 'Nat'.
data Row (r :: Nat) = Row

natToInt :: forall n. KnownNat n => Int
natToInt = fromIntegral (natVal @n Proxy)

square 
  :: forall n m a. (Elem a, KnownNat n, KnownNat m) 
  => Matrix n m a -> Bool
square _ = natToInt @n == natToInt @m


-- | The number of rows in the matrix
rows :: forall n m a. KnownNat n => Matrix n m a -> Int
rows _ = natToInt @n

-- | The number of colums in the matrix
cols :: forall n m a. KnownNat m => Matrix n m a -> Int
cols _ = natToInt @m

diagonal 
  :: (Elem a, KnownNat n, KnownNat m, r ~ Min n m, KnownNat r) 
  => Matrix n m a -> Matrix r 1 a
diagonal = _unop Internal.diagonal

length 
  :: forall n m a r. (Elem a, KnownNat n, KnownNat m, r ~ (n * m), KnownNat r) 
  => Matrix n m a -> Int
length _ = natToInt @r

-- | Return Matrix size as a pair of (rows, cols)
dims 
  :: forall n m a. (Elem a, KnownNat n, KnownNat m) 
  => Matrix n m a -> (Int, Int)
dims _ = (natToInt @n, natToInt @m)

coeff 
  :: forall n m a r c. (Elem a, KnownNat n, KnownNat r, KnownNat c, r <= n, c <= m) 
  => Row r -> Col c -> Matrix n m a -> a
coeff _ _ m@(Matrix (Vec vals)) =
  let !row  = natToInt @r
      !col  = natToInt @c
  in fromC $! VS.unsafeIndex vals $! col * rows m + row

unsafeCoeff :: (Elem a, KnownNat n) => Int -> Int -> Matrix n m a -> a
unsafeCoeff row col m@(Matrix (Vec vals)) = 
  fromC $! VS.unsafeIndex vals $! col * rows m + row




