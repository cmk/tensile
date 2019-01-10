
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
tensile - a purely functional n.n. API for TensorFlow, etc 
-----------------------------------------------------------------
target different frameworks & compute architectures 
research-prod parity
separate code from spec
higher-order functions act on graphs themselves
Neural Architecture Search (NAS)


multiple backends 
  - onnx-hmatrix
  - onnx-eigen 
  - onnx-accelerate    
    - https://github.com/AccelerateHS/accelerate
    - https://github.com/ekmett/linear-accelerate/
  - onnx-tensorflow
  - onnx-torch

  - tensile-onnx
  - tensile-tensorflow?



TOGROK

https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection#dynamically-constructing-type-class-instances
https://hackage.haskell.org/package/constraints-0.10.1/docs/Data-Constraint.html#t:Dict
http://hackage.haskell.org/package/reflection-extras-0.1.1.0/docs/Data-Reflection-Extras.html

with :: forall p a. Def p a -> (forall s. Reifies s (Def p a) => Lift p s a) -> a

using :: forall p a. ReifiableConstraint p => Def p a -> (p a => a) -> a
using (Monoid (+) 0) $ mempty <> 10 <> 12 -- 12
using (Gemm myGemm)
withOpSet 

-- use Constraints package to build a dict of op typeclasses

-- use this to define an OperatorSet (
--- http://neocontra.blogspot.com/2013/06/controlcategory-now-with-kind.html
--- https://kseo.github.io/posts/2017-02-06-reified-dictionaries.html
--- https://hackage.haskell.org/package/constraints-0.10.1/docs/src/Data.Constraint.html#Dict
--- https://github.com/onnx/onnx/blob/2e7099ee7c37b196c197c9a084a97698a41da232/onnx/onnx-operators.proto#L132


http://jelv.is/blog/Generating-Mazes-with-Inductive-Graphs/
https://hackage.haskell.org/package/dimensions-1.0.1.1/docs/Numeric-Dim.html
https://hackage.haskell.org/package/ad-4.3.5/docs/Numeric-AD.html


https://github.com/GU-CLASP/TypedFlow/blob/master/TypedFlow/Haskell.hs#L99
  - grok column: http://hackage.haskell.org/package/linear-1.20.8/docs/src/Linear.Matrix.html#column
  'column' is a generalization of 'Control.Lens.inside' to work over any corepresentable 'Functor'
  generalize to n-d slice

  - grok Distributive / Representable
  -- tabulated :: Representable f => Iso' (Rep f -> a) (f a)
  -- foldDimIdx :: Dims ds -- Shape of a space
           -> (Idxs ds -> a -> a) -- Function to call on each dimension
           -> a -- Initial value
           -> a

  - grok Conjoined, Indexable, Indexing, Index, Ixed 
http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Indexed.html 
http://hackage.haskell.org/package/lens-4.17/docs/Control-Lens-Internal-Indexed.html#t:Indexable
  - grok biplate / Data-Data-Lens

  - tf variables: https://github.com/tensorflow/community/blob/master/rfcs/20180817-variables-20.md#ref-edges-versus-resources


-- TODO: 

packages: onnx, tensile, tensile-onnx, tensile-tensorflow

-- see http://hackage.haskell.org/package/beam-core

-- tensile
Numeric.Tensile.Tensor
Numeric.Tensile.Expression (Expr)
Numeric.Tensile.Graph
Numeric.Tensile.

Numeric.Tensile.Backend.Linear


-- onnx-operators
Numeric.Tensile.Operators.NN

Numeric.Tensile.Backend.HMatrix 
-- use https://github.com/mstksg/hmatrix-backprop/blob/master/src/Numeric/LinearAlgebra/Static/Backprop.hs
--http://hackage.haskell.org/package/sparse
Numeric.Tensile.Backend.Sparse
Numeric.Tensile.Backend.Eigen
Numeric.Tensile.Backend.TensorFlow
Numeric.Tensile.Backend.Accelerate
Numeric.Tensile.Backend.ATen
Numeric.Tensile.Backend.Torch


ONNX TODO:
- basic proto manipulation. domain types.
https://github.com/onnx/onnx/blob/master/onnx/examples/Protobufs.ipynb


- use onnx 'supported opset' approach, but at typeclass/constraint level.
-- how do we take a model we made using hmatrix and run it on TF?
-- https://github.com/onnx/onnx/blob/master/docs/Versioning.md#operator-versioning 
-- https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection#dynamically-constructing-type-class-instances

> ONNX models declare which operator sets they require as a list of two part operator ids (domain, opset_version). The empty string ("") domain indicates the operators defined as part of the ONNX specification; other domains correspond to operator sets of other vendors (e.g., they can be used to provide vendor-specific extensions to ONNX). The union of the operator sets specified by a given model MUST have a compatible operator declaration for each node in the model's graph.

newtype Domain = Domain { unDomain :: Text }

-- https://github.com/onnx/onnx/blob/master/onnx/onnx.in.proto#L184
data Model = Model { graph :: Graph, meta :: [OperatorSetId], version :: Word }

-- https://github.com/onnx/onnx/blob/master/onnx/onnx.in.proto#L501
type OperatorSetId = (Domain, Version)

-- need a Kind for OpDefs
data OpSet where = '[Constraint] -- get version by folding over w/ max

class OpDef (* -> Constraint)
instance OpDef Gemm

class (TensorLike t, Transpose t) => Gemm t where

  domain :: Domain 
  domain = "" -- should be defaulted 
   
  type :: 
  type = 

  version :: Word
  version = 8 -- should be defaulted

  -- res = (v1 * M) + (v2 * mat1 * mat2)
  -- If @mat1@ is a @n × m@ matrix, @mat2@ a @m × p@ matrix, @M@ must be a @n × p@ matrix.
  gemm
    :: All KnownDim '[a, b, c]
    => HsReal                  -- ^ v1
    -> Tensor '[a, c]          -- ^ M
    -> HsReal                  -- ^ v2
    -> Tensor '[a, b]          -- ^ mat1
    -> Tensor '[b, c]          -- ^ mat2
    -> Tensor '[a, c]          -- ^ res

  gemm'

matmul
  :: (Gemm t, Constant t)
  => All KnownDim '[i, j, k]
  => KnownShape x
  => Num e
  => T t (i ': j ': x) e 
  -> T t (j ': k ': x) e 
  -> T t (i ': k ': x) e 
matmul x y = gemm 1 (constant 0) 1 x y

matmul' = (BVar version)

-- Dynamically construct a 'Foo' instance out of a supplied function.
withOrd :: (a -> a -> Ordering) -> (forall s. Reifies s (Ord_ a) => O a s) -> a
withOrd f v = reify (Ord_ f) (runO . asProxyOf v)
  where
    asProxyOf :: f s -> Proxy s -> f s
    asProxyOf v _ = v


-- replicate these
- (w/o IndexTensor) https://github.com/hasktorch/hasktorch/blob/master/indef/src/Torch/Indef/Index.hs
- https://github.com/hasktorch/hasktorch/blob/master/indef/src/Torch/Indef/Static/Tensor/Index.hs
- https://github.com/hasktorch/hasktorch/blob/master/indef/src/Torch/Indef/Static/Tensor.hs
- https://github.com/hasktorch/hasktorch/blob/master/indef/src/Torch/Indef/Static/Tensor/Math.hs
- https://github.com/hasktorch/hasktorch/blob/master/indef/src/Torch/Indef/Static/Tensor/Math/Reduce/Floating.hs https://github.com/hasktorch/hasktorch/blob/master/indef/src/Torch/Indef/Static/Tensor/Math/Reduce.hs


https://github.com/tensorflow/haskell/blob/8e1d85b5e5bd56d54ff6d463c8581c57ab5526d9/tensorflow/src/TensorFlow/BuildOp.hs
data ResultState = ResultState !OutputIx [Int64] deriving Show

-- | Class of types that can be used as op outputs.
class BuildResult a where
    buildResult :: ReaderT NodeName (State ResultState) a

class PureResult a where
    pureResult :: ReaderT (Build OpDef) (State ResultState) a

buildOp :: BuildResult a => [Int64] -> OpDef -> Build a
buildOp sizes o = do
    nodeName <- addNewOp o
    return $ flip evalState (ResultState 0 sizes) (runReaderT buildResult nodeName)


pureOp :: PureResult a => [Int64] -> Build OpDef -> a
pureOp sizes opDef = flip evalState (ResultState 0 sizes) (runReaderT pureResult opDef)


-- 
-- set up repo build script w/ import
-- define core T type. start with T (Tensor Build) s e rather than something more generic
-- define core typeclasses (see Beam / http://reasonablypolymorphic.com/blog/higher-kinded-data/)
-- figure out how to run gradBP on Variable nodes to keep graph size down
-- define Optimized typeclass. something like:

-- https://blog.jle.im/entry/purely-functional-typed-models-1.html
-- http://hackage.haskell.org/package/ad-4.3.5/docs/Numeric-AD.html#g:18
-- http://hackage.haskell.org/package/ad-4.3.5/docs/Numeric-AD-Rank1-Forward.html
class Backprop p => Optimizable p where
  optimize :: (Num b, Backprop b) => Model p a b -> p -> p

e.g.:
gradientDescent :: Optimizable p =>
gradientAscent
conjugateGradientDescent
stochasticGradientDescent
adam


-- define primary with* functions: 
   type T' d t s e = BVar d (T t s e)

   withFoo () => t -> (forall d s. (KnownShape s, Reifies d W) => T' s a -> r) -> r
   -- https://hackage.haskell.org/package/constraints-0.10.1/docs/Data-Constraint.html#t:Dict
   withDict :: Dict a -> (a => r) -> r


-- start creating grad instances. Q: can we do this purely w/in tensile?

-- explore giving tensors functor instances via co/yoneda & pusing fuctor application to creation (either via 'constant' or other operators/attributes). could we just use a free applicative for everything?
-- explore kmett/linear style using function applicatives. amazing that you can do matrix algebra w these.
-- grok adjuctions, yoneda. application to n-way currying. 
-- need hoist :: (forall a. a -> a) -> ([Word] -> a) -> (Word -> ... -> Word -> a)
-- or maybe density?
-- t s e = Idxs s -> e ~ n0 -> n1 -> ... -> nn -> e
-- generic currying (from f :: s  to  f :: n0 -> n1 -> ... -> nn -> e)
-- https://mail.haskell.org/pipermail/haskell/2004-May/014062.html
-- http://hackage.haskell.org/package/adjunctions-4.4/docs/Data-Functor-Adjunction.html






- make Elt kind / typeclass & Attribute kind / typeclass. Elt e => Attribute e 
-- https://github.com/onnx/onnx/blob/master/docs/IR.md#attribute-types
-- use Bits, Integral, RealFloat, RealFrac, Floating, Fractional, Real, Num

instance Bits Word -- Defined in ‘Data.Bits’
instance Bits Integer -- Defined in ‘Data.Bits’
instance Bits Int -- Defined in ‘Data.Bits’
instance Bits Bool -- Defined in ‘Data.Bits’

instance Integral Word -- Defined in ‘GHC.Real’
instance Integral Integer -- Defined in ‘GHC.Real’
instance Integral Int -- Defined in ‘GHC.Real’

instance RealFloat a => Num (Complex a)
instance Num Word -- Defined in ‘GHC.Num’
instance Num Integer -- Defined in ‘GHC.Num’
instance Num Int -- Defined in ‘GHC.Num’
instance Num Float -- Defined in ‘GHC.Float’
instance Num Double -- Defined in ‘GHC.Float’

instance Real Word -- Defined in ‘GHC.Real’
instance Real Integer -- Defined in ‘GHC.Real’
instance Real Int -- Defined in ‘GHC.Real’
instance Real Float -- Defined in ‘GHC.Float’
instance Real Double -- Defined in ‘GHC.Float’

instance RealFloat Float -- Defined in ‘GHC.Float’
instance RealFloat Double -- Defined in ‘GHC.Float’

instance RealFloat a => Floating (Complex a)
instance Floating Float -- Defined in ‘GHC.Float’
instance Floating Double 

instance RealFloat a => Fractional (Complex a)
instance Integral a => Fractional (Ratio a)
instance Fractional Float -- Defined in ‘GHC.Float’
instance Fractional Double -- Defined in ‘GHC.Float’
instance Class (Num a) (Fractional a)
instance Class (Fractional a) (Floating a)
instance RealFloat a :=> Fractional (Complex a)
instance Integral a :=> Fractional (Ratio a)

class (Real a, Fractional a) => RealFrac a where
  properFraction :: Integral b => a -> (b, a)
  truncate :: Integral b => a -> b
  round :: Integral b => a -> b
  ceiling :: Integral b => a -> b
  floor :: Integral b => a -> b
instance Integral a => RealFrac (Ratio a) -- Defined in ‘GHC.Real’
instance RealFrac Float -- Defined in ‘GHC.Float’
instance RealFrac Double -- Defined in ‘GHC.Float’
instance Integral a :=> RealFrac (Ratio a)


type I f s n = T f s (Int n)
type B f s = T f s Bool

type Scalar t = T '[] t


class (KnownLen s, All KnownNat s) => KnownShape s where

instance KnownShape '[]
instance (KnownNat x, KnownShape xs) => KnownShape (x ': xs)

class KnownTyp t where
  typVal :: Typ
class KnownBits t where
  bitsVal :: NBits

instance KnownBits 'B1 where bitsVal = B1
instance KnownBits 'B32 where bitsVal = B32
instance KnownBits 'B64 where bitsVal = B64
instance (KnownBits l, KnownKind k) => KnownTyp ('Typ k l) where
  typVal = Typ (kindVal @k) (bitsVal @l)

class KnownKind t where
  kindVal :: Kind

instance KnownKind 'Bool where kindVal = Bool
instance KnownKind 'Float where kindVal = Float
instance KnownKind 'Int where kindVal = Int



- Expr idea
-- https://hackage.haskell.org/package/typedflow-0.9/docs/TypedFlow-Types.html#t:GState
-- https://www.tensorflow.org/guide/extend/model_files
-- https://hackage.haskell.org/package/simple-reflect instances?
 
- decide how to handle op attributes (put in function args where possible)
-- https://github.com/tensorflow/tensorflow/blob/master/tensorflow/core/framework/attr_value.proto

- make onnx-proto, compile onnx protos w/ proto-lens, compare w/ tf versions
https://github.com/onnx/onnx/blob/765f5ee823a67a866f4bd28a9860e81f3c811ce8/onnx/onnx.proto
https://tensorflow.github.io/haskell/haddock/tensorflow-proto-0.2.0.0/



- define datatypes:

-- https://github.com/travitch/haggle/blob/master/src/Data/Graph/Haggle.hs
-- note that TF nodes do not record outputs in this type, but onnx nodes do. we follow onnx here but will make a Rendered typeclass : class Rendered s where MonadState s m => Expr a -> m a
-- later fold Expr to TF NodeDef, Tensor Build a and eval with Build.hs
-- we use the functions BELOW in the Expr interpreter
newtype Expr t = Expr { unExpr :: FGL.Gr Operator t } 


-- node https://github.com/onnx/onnx/blob/master/docs/IR.md#nodes
-- Node dependencies MUST NOT create cycles in the computation graph. HOW TO ENFORCE?
newtype Node t = Node { unNode :: FGL.Context Operator t }


-- t above can be TRep or FRep. note these are adjoint!!!
-- domain type for https://tensorflow.github.io/haskell/haddock/tensorflow-proto-0.2.0.0/Proto-Tensorflow-Core-Framework-TensorDescription.html
-- http://hackage.haskell.org/package/comonad-5.0.2/docs/Control-Comonad-Trans-Env.html#t:EnvT
data TRep = TRep { shape :: [Word], type :: TType } ~ Env [Word] TType 
newtype FRep = FRep { unFRep :: [Word] -> TType } ~ Reader [Word] TType 


-- useful?
-- https://hackage.haskell.org/package/dimensions-1.0.1.1/docs/src/Numeric.Dimensions.Idxs.html#idxsFromWords
forall ds . Dimensions ds => Prism (TRep a) (T TRep (Idx ds) a)



-- https://tensorflow.github.io/haskell/haddock/tensorflow-proto-0.2.0.0/Proto-Tensorflow-Core-Framework-OpDef.html#t:OpDef
-- see also https://tensorflow.github.io/haskell/haddock/tensorflow-0.2.0.0/src/TensorFlow.Output.html#OpDef
-- https://tensorflow.github.io/haskell/haddock/tensorflow-proto-0.2.0.0/Proto-Tensorflow-Core-Framework-NodeDef.html
-- https://github.com/onnx/onnx/blob/master/onnx/onnx.proto3#L279
-- https://github.com/onnx/onnx/blob/master/docs/IR.md#nodes
-- https://github.com/onnx/onnx/blob/master/docs/IR.md#attributes
-- properties: Operator arity should match Context, Operator shape / type should match that of the f's
data Operator 


-- tensor (for tf dense/sparse, onnx dense, hmatrix, etc)
newtype T (t :: * -> *) (s :: Dims) e = T { unT :: t e }
type T' t s e = BVar s (T t s e)
-- tabulated :: Representable f => Iso' (Rep f -> a) (f a)

-- tensor shape
https://github.com/onnx/onnx/blob/master/docs/IR.md#tensor-shapes
https://hackage.haskell.org/package/dimensions-1.0.1.1/docs/src/Numeric.Dim.html#XNat
https://hackage.haskell.org/package/dimensions-1.0.1.1/docs/src/Numeric.Dimensions.Dims.html#Dims

--https://github.com/onnx/onnx/blob/master/onnx/onnx.proto#L419
message TensorShapeProto {
  message Dimension { 
    oneof value { // XNat
      int64 dim_value = 1; // N
      string dim_param = 2; // XN
    };
  };
  repeated Dimension dim = 1; //Dims = TypedList Dim xs
}

message Tensor {
    optional TensorProto.DataType elem_type = 1;
    optional TensorShapeProto shape = 2;
  }




- define T f s e instance for Backprop, Num, Fractional, and Floating
instance Elt e, NotBool e => Num e where
instance Elt e, NotBool e, NotInt e => Fractional e where
instance Elt e, NotBool e, NotInt e => Floating e where

- use dimensions lib to implement bcast: 
https://hackage.haskell.org/package/dimensions-1.0.1.1/
https://github.com/GU-CLASP/TypedFlow/blob/9f053e9cb8ee54aed411fb6c7d93eb29d28a6862/TypedFlow/Abstract.hs

- need to add Op defns for every method in Num, Fractional, and Floating. see https://github.com/mstksg/backprop/blob/master/src/Numeric/Backprop/Op.hs?

- define remaining typeclasses w/ defaults and functor => instances (from linear / test.hs)
-- basically gotta use the opdefs to implement all these type classes
-- https://github.com/onnx/onnx/blob/master/docs/Operators.md


 
-- https://github.com/onnx/onnx/blob/master/docs/Operators.md#max
instance Ord e => Ord T e where

MonadBuild

  https://github.com/onnx/onnx/blob/master/docs/Operators.md#Loop
  loop


https://github.com/onnx/onnx/blob/master/docs/Operators.md#if
elseThenIf

instances: Eq, Ord, Num, Fractional, Floating

class Boolean where
  and &&&
  or  |||
  not
  xor 

class Reduce where
ReduceL1
ReduceL2
ReduceLogSum
ReduceLogSumExp
ReduceMax
ReduceMean
ReduceMin
ReduceProd
ReduceSum
ReduceSumSquare


class Dims v => Finite v where

  type Size v :: Nat -- this should allow kind k, for Reifies k Int
  toV :: v a -> V (Size v) a
  default toV :: Foldable v => v a -> V (Size v) a

  toV = V . V.fromList . Foldable.toList
  fromV :: V (Size v) a -> v a

  -- need to get shape
  fill :: V (Size v) a -> v a

  unstack :: t (n:s) a -> V n (t s a)

-- Numeric.Tensile.NN.Tensor

-- let t hold s instead so we can use hmatrix-static, hasktorch, etc ?
-- see https://github.com/hasktorch/hasktorch/blob/master/indef/src/Torch/Indef/Static/Tensor/Math/Blas.hs
class Tensor t where 

  shape :: (KnownShape s, Len s ~ n, Elt e) => T t s e -> V n Int
  shape = 

  constant :: (KnownShape s, Len s ~ n, Size s ~ n', Elt e) => V n' e -> T t s e
  -- constant v = fill $ const v

  -- forces a denotation of data ordering (e.g. row-major, col-major etc)
  fill :: (Idxs s -> e) -> T t s e


  -- res = (v1 * M) + (v2 * mat1 * mat2)
  -- If @mat1@ is a @n × m@ matrix, @mat2@ a @m × p@ matrix, @M@ must be a @n × p@ matrix.
  gemm
    :: All KnownDim '[a, b, c]
    => HsReal                  -- ^ v1
    -> Tensor '[a, c]          -- ^ M
    -> HsReal                  -- ^ v2
    -> Tensor '[a, b]          -- ^ mat1
    -> Tensor '[b, c]          -- ^ mat2
    -> Tensor '[a, c]          -- ^ res

  matmul
    :: All KnownDim '[i, j, k]
    => KnownShape x
    => Num e
    => T t (i ': j ': x) e 
    -> T t (j ': k ': x) e 
    -> T t (i ': k ': x) e 
  matmul x y = gemm 1 (constant 0) 1 x y

  -- Tensor (Kronecker) product 
  -- transpose (m ⊗ n) == (transpose m) ⊗ (transpose n)
  -- adjoint (m ⊗ n) == (adjoint m) ⊗ (adjoint n)
  kronecker :: t s a -> t s' a -> t (s++s') a 

  transpose :: Rank 1 f, Tensor g => f * g -> g * f

  -- f has rank 1
  transpose :: Tensor g => f (g a) -> g (f a)

  -- https://github.com/ekmett/linear/blob/master/src/Linear/Trace.hs
  trace :: f (g (h a)) -> h a

  -- contract 




-- | infix 'matmul'

(#) :: T t (i ': j ': x) e -> T t (j ': k ': x) e -> T t (i ': k ': x) e 
(#) = matmul

(<#>)
dot :: KnownShape s, Num e => T t s e -> T t s e -> e
dot a b = reduce $ a `mul` b

(#>) :: (All KnownDim '[r, c]) => Tensor '[r, c] -> Tensor '[c] -> Tensor '[r]
(#>) a b = mv a b



https://github.com/hasktorch/hasktorch/blob/master/indef/src/Torch/Indef/Static/Tensor/Math/Pairwise.hs

-- use bcast instead
class Num real => Pairwise tensor real where
  -- | infix version of 'add'
  (#.)
  (^+) :: tensor -> real -> tensor
  -- | infix version of 'sub'
  (^-) :: tensor -> real -> tensor
  -- | infix version of 'mul'
  (^*) :: tensor -> real -> tensor
  -- | infix version of 'div'
  (^/) :: tensor -> real -> tensor


infixl 7 ^*,^/
infixl 6 ^+,^-

(^+) = bcast ...
(^-) = bcast
(^*) = mul
(^/) = div

-- | flipped version of '(^+)'
(+^) :: Pairwise ten real => real -> ten -> ten
(+^) = flip (^+)

-- | flipped version of '(^*)'
(*^) :: Pairwise ten real => real -> ten -> ten
(*^) = flip (^*)

-- | flipped version of '(^/)'
(/^) :: Pairwise ten real => real -> ten -> ten
(/^) = flip (^/)

-- | flipped version of '(^-)'
(-^) :: forall ten real . Pairwise ten real => real -> ten -> ten
(-^) v t = v +^ (negate (1::real) *^ t)


https://github.com/hasktorch/hasktorch/blob/master/indef/src/Torch/Indef/Static/Tensor/Math/Pointwise.hs
infixl 6 ^+^, ^-^
infixl 7 ^*^, ^/^


-- Numeric.Tensile.Backend
-- see also https://github.com/onnx/onnx/blob/master/docs/Operators.md
-- add scatter / gather?
-- add flatten https://github.com/onnx/onnx/blob/master/docs/Operators.md#Flatten
-- add expand / compress?
-- add squeeze / unsqueeze? https://github.com/onnx/onnx/blob/master/docs/Operators.md#Squeeze
class (Tensor t, Shape s, Elt e) => Tensile (t s e) where

  -- s1234 have optic-like relationship, 
  -- easy to impl w/ Fuctor/Foldable w/ Index
  tmap :: (t s1 a -> t s2 a) -> t s3 a -> t s4 a
  https://github.com/AccelerateHS/accelerate/blob/dc743a12753f3cd22c6f5184576112ed801e682b/src/Data/Array/Accelerate/Data/Functor.hs#L48
  http://hackage.haskell.org/package/linear-accelerate-0.6.0.0/docs/src/Data-Array-Accelerate-Linear-V1.html#line-154
  tmap :: (t n a -> t s2 a) -> t s3 a -> t s4 a

  -- transpose :: f (g a) -> g (f a)
  -- https://github.com/GU-CLASP/TypedFlow/blob/9f053e9cb8ee54aed411fb6c7d93eb29d28a6862/TypedFlow/Abstract.hs#L245
  transpose :: (Perm s s') => t s a -> t s' a

  slice :: (In s s') => t s e -> t s' e

  shape :: (Integral i, Nat n, n ~ Size s) t s e -> t n i

  -- https://github.com/onnx/onnx/blob/master/docs/Operators.md#Reshape
  -- https://github.com/GU-CLASP/TypedFlow/blob/9f053e9cb8ee54aed411fb6c7d93eb29d28a6862/TypedFlow/Abstract.hs#L266
  reshape :: (Size s ~ Size s') => t s e -> t s' e

  -- https://github.com/onnx/onnx/blob/master/docs/Operators.md#Tile
  tile ::

  -- like tf.stack
  -- https://github.com/onnx/onnx/blob/master/docs/Operators.md#concat
  concat :: V n (t s a) -> t (n:s) a


  -- https://www.tensorflow.org/xla/broadcasting
  -- https://github.com/onnx/onnx/blob/master/docs/Broadcasting.md
  -- Need a recursive typelevel function for Comp
  -- https://github.com/onnx/onnx/blob/master/docs/Operators.md#Expand
  bcast (Comp s s') :: t s e -> t s' e


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




-- https://hackage.haskell.org/package/uniform-pair-0.1.15/docs/src/Data.UniformPair.html#line-135

------------------------------------------------------------------
{-# LANGUAGE GADTs, DataKinds, PolyKinds, TypeFamilies, TypeOperators #-}
-- https://www.reddit.com/r/haskell/comments/67l9so/currying_a_typelevel_list/
-- http://hackage.haskell.org/package/singletons-2.2/docs/Data-Singletons-Prelude-Tuple.html#t:Curry
module Curry where

import Data.Kind

type family Curry (xs :: [Type]) (y :: Type) where
  Curry '[] y = y
  Curry (x ': xs) y = x -> Curry xs y

data Record :: (k -> Type) -> [k] -> Type where
  RecordNil :: Record f '[]
  RecordCons :: f a -> Record f as -> Record f (a ': as)

newtype Identity a = Identity a

curryRecord' :: (Record Identity as -> r) -> Record proxy as -> Curry as r
curryRecord' k RecordNil = k RecordNil
curryRecord' k (RecordCons _ r0) = \a -> curryRecord' (\r -> k (RecordCons (Identity a) r)) r0

curryRecord :: Record proxy as -> Curry as (Record Identity as)
curryRecord = curryRecord' id


------------------------------------------------------------------

https://ro-che.info/articles/2013-01-29-generic-uncurry

https://mail.haskell.org/pipermail/haskell/2004-May/014062.html

:set -XFlexibleInstances
:set -XFunctionalDependencies
:set -XMultiParamTypeClasses
:set -XUndecidableInstances

-- An uncurrying application: flattening the product
-- http://hackage.haskell.org/package/product-profunctors-0.10.0.0/docs/Data-Profunctor-Product-Flatten.html
class FApp f a c | a c -> f where fapp:: f -> a -> c
   
instance FApp (Char->c) Char c where fapp = ($)

instance (FApp fx a r, FApp r b c) => FApp fx (a,b) c where fapp f = uncurry (fapp . (fapp f))

-- test
fappt :: [Char] --"abcde"
fappt = fapp (\a b c d e -> [a,b,c,d,e]) (('a','b'),('c',('d','e')))


Combinators fapp and mcomp will occur together. Therefore, we define

mcomp f g = g . f

fcomp :: FApp f a1 c => (a2 -> a1) -> f -> a2 -> c
fcomp a = (. fapp) (mcomp a)


class (Functor path, Functor space) =>
		Adjunction path space | path -> space, space -> path where
      leftAdjunct :: (path top -> bot) -> top -> space bot
      unit :: top -> space (path top) 
      rightAdjunct :: (top -> space bot) -> path top -> bot
      counit :: path (space bot) -> bot 
      -- minimum required impl: unit xor leftAdjunct
      -- minimum required impl: counit xor rightAdjunct
      unit = leftAdjunct id
      leftAdjunct f = fmap f . unit
      counit = rightAdjunct id
      rightAdjunct g = counit . fmap g

-- Here are some instances for different arities:

instance Adjunction ((,) a) ((->) a) where
	 unit t = \arg -> (arg,t)
	 counit (x,f) = f x

newtype Func2 a b c = Func2 (a -> b -> c) deriving Functor
   -- Func2 is only needed due to syntax of partial type constructor application

instance Adjunction ((,,) a b) (Func2 a b) where
	  unit t = Func2 (\arg1 arg2 -> (arg1,arg2,t))
	  counit (arg1,arg2,Func2 f) = f arg1 arg2


Here, 'leftAdjunct' is a generalization of curry and rightAdjunct is a
generalization of uncurry.


import Data.Tuple

fst3 :: (a,b,c) -> a
fst3 (x,y,z) = x

snd3 :: (a,b,c) -> b
snd3 (x,y,z) = y

trd3 :: (a,b,c) -> c
trd3 (x,y,z) = z

fst4 :: (a,b,c,d) -> a
fst4 (x,y,z,w) = x

snd4 :: (a,b,c,d) -> b
snd4 (x,y,z,w) = y

trd4 :: (a,b,c,d) -> c
trd4 (x,y,z,w) = z

fth4 :: (a,b,c,d) -> d
fth4 (x,y,z,w) = w

curry3 :: ((a,b,c)->d) -> a -> b -> c -> d
curry3 f x y z = f (x,y,z)

curry4 :: ((a,b,c,d)->e) -> a -> b -> c -> d -> e
curry4 f x y z w = f (x,y,z,w)

uncurry3 :: (a->b->c->d) -> (a,b,c) -> d
uncurry3 f t = f (fst3 t) (snd3 t) (trd3 t)

uncurry4 :: (a->b->c->d->e) -> (a,b,c,d) -> e
uncurry4 f q = f (fst4 q) (snd4 q) (trd4 q) (fth4 q)

uncurry5 :: (a->b->c->d->e->f) -> (a,b,c,d,e) -> f
uncurry5 f (x,y,z,w,v) = f x y z w v

uncurry6 :: (a->b->c->d->e->f->g) -> (a,b,c,d,e,f) -> g
uncurry6 f (x,y,z,w,v,u) = f x y z w v u

uncurry7 :: (a->b->c->d->e->f->g->h) -> (a,b,c,d,e,f,g) -> h
uncurry7 f (x,y,z,w,v,u,r) = f x y z w v u r

uncurry8 :: (a->b->c->d->e->f->g->h->i) -> (a,b,c,d,e,f,g,h) -> i
uncurry8 f (x,y,z,w,v,u,r,s) = f x y z w v u r s

uncurry9 :: (a->b->c->d->e->f->g->h->i->j) -> (a,b,c,d,e,f,g,h,i) -> j
uncurry9 f (x,y,z,w,v,u,r,s,t) = f x y z w v u r s t

uncurry10 :: (a->b->c->d->e->f->g->h->i->j->k) -> (a,b,c,d,e,f,g,h,i,j) -> k
uncurry10 f (x,y,z,w,v,u,r,s,t,o) = f x y z w v u r s t o

uncurry11 :: (a->b->c->d->e->f->g->h->i->j->k->l)
          -> (a,b,c,d,e,f,g,h,i,j,k) -> l
uncurry11 f (x,y,z,w,v,u,r,s,t,o,p) = f x y z w v u r s t o p

uncurry12 :: (a->b->c->d->e->f->g->h->i->j->k->l->m)
          -> (a,b,c,d,e,f,g,h,i,j,k,l) -> m
uncurry12 f (x,y,z,w,v,u,r,s,t,o,p,q) = f x y z w v u r s t o p q
