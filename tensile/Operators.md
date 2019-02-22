https://github.com/helq/tensorflow-haskell-deptyped/blob/master/library/TensorFlow/DepTyped/Ops.hs

https://www.tensorflow.org/versions/r2.0/api_docs/python/tf
https://pytorch.org/docs/stable/tensors.html

Kolda et al - Tensor Decompositions and Applications
http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=4A0663C7848627DADDBA6A243BC43E78?doi=10.1.1.130.782&rep=rep1&type=pdf


git grep -l 'Generators' | xargs sed -i '' -e 's/Generators/Gen/g'

audio module: Public API for tf.audio namespace.

autograph module: Conversion of plain Python into TensorFlow graph code.

bitwise module: Operations for manipulating the binary representations of integers.

compat module: Functions for Python 2 vs. 3 compatibility.

config module: Public API for tf.config namespace.

data module: tf.data.Dataset API for input pipelines.

debugging module: Public API for tf.debugging namespace.

distribute module: Library for running a computation across multiple devices.

dtypes module: Public API for tf.dtypes namespace.

errors module: Exception types for TensorFlow errors.

estimator module: Estimator: High level tools for working with models.

experimental module: Public API for tf.experimental namespace.

feature_column module: Public API for tf.feature_column namespace.

graph_util module: Helpers to manipulate a tensor graph in python.

image module: Image processing and decoding ops.

io module: Public API for tf.io namespace.

keras module: Implementation of the Keras API meant to be a high-level API for TensorFlow.

linalg module: Operations for linear algebra.

lite module: Public API for tf.lite namespace.

losses module: Loss operations for use in neural networks.

math module: Math Operations.

nest module: Public API for tf.nest namespace.

nn module: Wrappers for primitive Neural Net (NN) Operations.

quantization module: Public API for tf.quantization namespace.

queue module: Public API for tf.queue namespace.

ragged module: Ragged Tensors.

random module: Public API for tf.random namespace.

raw_ops module: Public API for tf.raw_ops namespace.

rnn module: Public API for tf.rnn namespace.

saved_model module: Public API for tf.saved_model namespace.

sets module: Tensorflow set operations.

signal module: Signal processing operations.

sparse module: Sparse Tensor Representation.

strings module: Operations for working with string Tensors.

summary module: Operations for writing summary data, for use in analysis and visualization.

sysconfig module: System configuration library.

test module: Testing.

tools module

train module: Support for training models.

version module: Public API for tf.version namespace.

scalar (tf.math)
vector (tf.linalg)

ONNX: use cabal libraries & versions to track?

ai.onnx (default)
Abs
Acos
Acosh
Add
And
ArgMax
ArgMin
Asin
Asinh
Atan
Atanh
AveragePool
BatchNormalization
Cast
Ceil
Clip
Compress
Concat
Constant
ConstantOfShape
Conv
ConvTranspose
Cos
Cosh
DepthToSpace
Div
Dropout
Elu
Equal
Erf
Exp
Expand
EyeLike
Flatten
Floor
GRU
Gather
Gemm
GlobalAveragePool
GlobalLpPool
GlobalMaxPool
Greater
HardSigmoid
Hardmax
Identity
If
InstanceNormalization
IsNaN
LRN
LSTM
LeakyRelu
Less
Log
LogSoftmax
Loop
LpNormalization
LpPool
MatMul
Max
MaxPool
MaxRoiPool
MaxUnpool
Mean
Min
Mul
Multinomial
Neg
NonZero
Not
OneHot
Or
PRelu
Pad
Pow
RNN
RandomNormal
RandomNormalLike
RandomUniform
RandomUniformLike
Reciprocal

Relu
Reshape
Scan
Scatter
Selu
Shape
Shrink
Sigmoid
Sign
Sin
Sinh
Size
Slice
Softmax
Softplus
Softsign
SpaceToDepth
Split
Sqrt
Squeeze
Sub
Sum
Tan
Tanh
Tile
TopK
Transpose
Unsqueeze
Upsample
Where
Xor


expandDims Source#

:: (TensorType t, OneOf '[Int32, Int64] tdim)	 
=> Tensor v'1 t	

-> Tensor v'2 tdim	

-> Tensor Build t

typed-flow
  slice, slice0, slice1,
  litStack0,
  stack0, unstack0,
  stack1,
  concatT, concat0, concat1,
  -- ** Reshaping
  expandDim,
  expandDim0, squeeze0,
  expandDim1, 
  flatten2, flatten3, flatten12, flattenN2,
  inflate2, inflate3, inflate12,
  reshape, flattenAll, inflateAll,


tf.split(
    value,
    num_or_size_splits,
    axis=0,
    num=None,
    name='split'
)

Defined in tensorflow/python/ops/array_ops.py.

Splits a tensor into sub tensors.

If num_or_size_splits is an integer type, then value is split along dimension axis into num_split smaller tensors. Requires that num_split evenly divides value.shape[axis].

If num_or_size_splits is not an integer type, it is presumed to be a Tensor size_splits, then splits value into len(size_splits) pieces. The shape of the i-th piece has the same size as the value except along dimension axis where the size is size_splits[i].








slice:
t = tf.constant([[[1, 1, 1], [2, 2, 2]],
                 [[3, 3, 3], [4, 4, 4]],
                 [[5, 5, 5], [6, 6, 6]]]) 3 x 2 x 3
tf.slice(t, [1, 0, 0], [1, 1, 3])  # [[[3, 3, 3]]]
tf.slice(t, [1, 0, 0], [1, 2, 3])  # [[[3, 3, 3],
                                   #   [4, 4, 4]]]
tf.slice(t, [1, 0, 0], [2, 1, 3])  # [[[3, 3, 3]],
                                   #  [[5, 5, 5]]]

-- want d' < d, 
slice :: Idxs d -> Dims d' -> Tensor d e -> Tensor d' e

slice :: (TensorType t, OneOf '[Int32, Int64] index)	=> Tensor v'1 t	-> Tensor v'2 index	-> Tensor v'3 index	-> Tensor Build t

Idxs d -> I '[Rank d]

	
-- | Take a slice at dimension n from i to j.
slice :: forall i j s t n. KnownTyp t => KnownShape s => KnownNat j => KnownNat i => (i <= j, j <= At n s, KnownLen s) =>
         Axis n s -> Tensor s t -> Tensor (Take n s ++ ((j-i) ': Drop ('Succ n) s)) t
slice n = case axisSplitApp' n of
  Refl -> UnOp (SliceOp (Proxy @(j-i)) (hlookup n s) (sShapeDropSucc n s) (natVal (Proxy @i)) (natVal (Proxy @j)))
               (sShapeTake' n s)
 where s = typeSShape @s


slice1 :: forall i j m n s t. KnownShape s => KnownNat m => KnownNat n => KnownTyp t => KnownNat j => KnownNat i => (i <= j, j <= m, KnownLen s) =>
         Tensor (n ': m ': s) t -> Tensor (n ': (j-i) ': s) t
slice1 = slice @i @j axis1

slice0 :: forall i j m s t. KnownShape s => KnownNat m => KnownTyp t => KnownNat j => KnownNat i => (i <= j, j <= m, KnownLen s) =>
         Tensor (m ': s) t -> Tensor ((j-i) ': s) t
slice0 = slice @i @j axis0



-- pack / stack
https://www.tensorflow.org/api_docs/python/tf/stack

pack :: TensorType t	=> [Tensor v'1 t]	-> Tensor Build t	

unpack :: TensorType t	=> Int64 -> Tensor v'1 t -> [Tensor Build t]	

stack ::  Dim n -> Vector n (Tensor (x ++ y) e) -> Tensor (x +: n :+ y) e
unstack :: (KnownDims x, Elt e) => Dim n -> Tensor (x +: n :+ y) e -> Vector n (Tensor (x ++ y) e)


https://github.com/hasktorch/hasktorch/blob/c57e828e132a16aed9d87d9a7e98fe3145da459d/indef/src/Torch/Indef/Static/Tensor/Math.hs#L136

cat
  :: '(ls, r0:+rs) ~ Sing.SplitAt i d
  => '(ls, r1:+rs) ~ Sing.SplitAt i d'
  => Tensor d
  -> Tensor d'
  -> Dim (i::Nat)
  -> Tensor (ls ++ '[r0 + r1] ++ rs)

cat1d
  :: (All KnownDim '[n1,n2,n], n ~ Sing.Sum [n1, n2])
  => Tensor '[n1] -> Tensor '[n2] -> Tensor '[n]
cat1d a b = cat a b (dim :: Dim 0)

-- | convenience function, specifying a type-safe 'cat' operation.
cat2d0 :: (All KnownDim '[n,m,n0,n1], n ~ Sing.Sum [n0, n1]) => Tensor '[n0, m] -> Tensor '[n1, m] -> Tensor '[n, m]
cat2d0 a b = cat a b (dim :: Dim 0)

-- | convenience function, stack two rank-1 tensors along the 0-dimension
stack1d0 :: KnownDim m => Tensor '[m] -> Tensor '[m] -> (Tensor '[2, m])
stack1d0 a b = cat2d0
  (unsqueeze1d (dim :: Dim 0) a)
  (unsqueeze1d (dim :: Dim 0) b)


stackT :: ∀ s0 s (n::Nat) t. KnownShape s => KnownShape s0 => KnownNat n => (KnownLen s0) => V n (T (s0 ++ s) t) -> Tensor (s0 ++ (n ': s)) t
stackT v = vecToNP @(T (s0++s) t) @(Catable s0 s t)
             (\x -> (Catable (natSat @1) $ prodHomoS s0 s
                                          $ prodHomoS s0 (natSat @1 :+ s)
                                          $ knownAppend @s0 @s
                                          $ knownSShape (s0 .+. natSat @1 :+ s)
                                          $ reshape x))
             v $ (Concat (typeSShape @s0)  (typeSShape @s)) 
  where s = typeSShape @s; s0 = typeSShape @s0


-- | Concatenate @n@ tensors along the first dimension
stack0 :: ∀ s (n::Nat) t. KnownNat n => KnownShape s => (KnownLen s) => V n (T s t) -> Tensor (n ': s) t
stack0 = stackT @'[]

-- | Concatenate @n@ tensors along the second dimension
stack1 :: ∀ s (n::Nat) m t. KnownNat n => KnownNat m => KnownShape s => (KnownLen s) => V n (T (m ': s) t) -> Tensor (m ': n ': s) t
stack1 = stackT @'[m]

-- | Concatenate @n@ tensors along the last dimension
stackN :: ∀ s (n::Nat) t. KnownNat n => KnownShape s => V n (T s t) -> Tensor (s ++ '[n]) t
stackN = appRUnit @s $
         stackT @s @'[]


-- | Split a tensors into @n@ tensors along the first dimension
unstack0 :: ∀ s (n::Nat) t. KnownTyp t => KnownNat n => KnownShape s => (KnownLen s) => Tensor (n ': s) t -> V n (T s t)
unstack0 x = fmap (`nth0` x) (vcount @n)



-- | Static call to 'Dynamic._gather'
_gather :: Tensor d -> Tensor d -> Word -> IndexTensor '[n] -> IO ()
_gather r src d ix = Dynamic._gather (asDynamic r) (asDynamic src) d (longAsDynamic ix)

-- | Static call to 'Dynamic._scatter'
_scatter :: Tensor d -> Word -> IndexTensor '[n] -> Tensor d -> IO ()
_scatter r d ix src = Dynamic._scatter (asDynamic r) d (longAsDynamic ix) (asDynamic src)

-- | Static call to 'Dynamic._scatterAdd'
_scatterAdd   :: Tensor d -> Word -> IndexTensor '[n] -> Tensor d -> IO ()
_scatterAdd r d ix src = Dynamic._scatterAdd (asDynamic r) d (longAsDynamic ix) (asDynamic src)

-- | Static call to 'Dynamic._scatterFill'
_scatterFill  :: Tensor d -> Word -> IndexTensor '[n] -> HsReal -> IO ()
_scatterFill r d ix = Dynamic._scatterFill (asDynamic r) d (longAsDynamic ix)



-- | Performs the dot product between two tensors. The number of elements must match: both tensors are
-- seen as a 1D vector.
dot :: All Dimensions '[d,d'] => Tensor d -> Tensor d' -> HsAccReal
dot a b = Dynamic.dot (asDynamic a) (asDynamic b)

-- | 'addr' with the parameters for an outer product filled in.
outer
  :: forall t r c . (All KnownDim '[r, c])
  => Tensor '[r] -> Tensor '[c] -> Tensor '[r, c]
outer v1 v2 = addr 0 (constant 0) 1 v1 v2

-- | added simplified use of addmv: src1 #> src2
mv
  :: (All KnownDim '[r, c])
  => Tensor '[r, c] -> Tensor '[c] -> Tensor '[r]
mv m v = addmv 0 (constant 0) 1 m v

-- | simplified wrapper of 'addmm'
--
-- FIXME: see if we can pass a null pointer in as the constant value (which might eliminate a noop linear pass).
mmult
  :: All KnownDim '[a, b, c]
  => T '[a, b] e
  -> T '[b, c] e
  -> T '[a, c] e
mmult x y = addmm 1 (constant 0) 1 x y



-- | Static call to 'Dynamic.onesLike'
onesLike :: forall d . Dimensions d => (Tensor d)
onesLike = asStatic $ Dynamic.onesLike (dims :: Dims d)

-- | Static call to 'Dynamic.zerosLike'
zerosLike :: forall d . Dimensions d => (Tensor d)
zerosLike = asStatic $ Dynamic.zerosLike (dims :: Dims d)


matrix
  :: forall n m
  .  (All KnownDim '[n, m], All KnownNat '[n, m])
#if MIN_VERSION_singletons(2,4,0)
  => KnownDim (n*m) => KnownNat (n*m)
#else
  => KnownDim (n*:m) => KnownNat (n*:m)
#endif
  => [[HsReal]] -> ExceptT String IO (Tensor '[n, m])
matrix ls

-- | Expand a vector by copying into a matrix by set dimensions
-- TODO - generalize this beyond the matrix case
expand2d
  :: forall x y . (All KnownDim '[x, y])
  => Tensor '[x] -> Tensor '[y, x]
expand2d t = unsafeDupablePerformIO $ do

eye :: forall d1 d2 . (All KnownDim '[d1, d2]) => Tensor '[d1, d2]
eye = asStatic $ Dynamic.eye (dim :: Dim d1) (dim :: Dim d2)

-- | Static call to 'Dynamic.constant'
constant :: forall d . Dimensions d => HsReal -> Tensor d
constant = asStatic . Dynamic.constant (dims :: Dims d)

-- | Static call to 'Dynamic.diag'
-- | returns the k-th diagonal of the input tensor, where k=0 is the main diagonal,
-- k>0 is above the main diagonal, and k<0 is below the main diagonal.
-- cat2d0 :: (All KnownDim '[n,m,n0,n1], n ~ Sing.Sum [n0, n1]) => Tensor '[n0, m] -> Tensor '[n1, m] -> Tensor '[n, m]
diag :: forall k d d' . (All KnownDim '[d, k], d' ~ Sing.Sum [d, k])
     => Tensor '[d] -> Dim (k::Nat) -> DiagDir -> Tensor [d', d']
diag t offset diagDir = asStatic $ Dynamic.diag (asDynamic t) offset'
  where
    offset' = case diagDir of
      DiagAbove -> fromIntegral $ fromDim offset
      DiagBelow -> -(fromIntegral $ fromDim offset)

-- | Create a diagonal matrix from a 1D vector
diag1d :: (KnownDim n) => Tensor '[n] -> Tensor '[n, n]
diag1d t = diagUnsafe t 0




----- shape manipulation

getDim
  :: forall d i d'
  .  All Dimensions '[d, i:+d']
  => Tensor (d::[Nat])
  -> Dims ((i:+d')::[Nat]) -- ^ the index to get is a non-empty dims list
  -> Maybe HsReal
getDim t d = Dynamic.getDim (asDynamic t) d

-- | Takes a 'Data.Vector.Generic.Vector' and returns a continuation
-- providing a 'Data.Vector.Generic.Sized' with a size parameter @n@ that
-- is determined at runtime based on the length of the input vector.
--
-- Essentially converts a 'Data.Vector.Generic.Vector' into
-- a 'Data.Vector.Generic.Sized.Vector' with the correct size parameter
-- @n@.
withSized :: forall v a r. VG.Vector v a
          => v a
          -> (forall n. KnownNat n => Vector v n a -> r) -> r
withSized v f = case someNatVal (fromIntegral (VG.length v)) of
  Just (SomeNat (Proxy :: Proxy n)) -> f (Vector v :: Vector v n a)
  Nothing -> error "impossible: Vector has negative length"


http://hackage.haskell.org/package/vector-sized-1.2.0.0/docs/Data-Vector-Sized.html#v:SomeSized
-- | /O(1)/ Reveal a 'KnownNat' instance for a vector's length, determined
-- at runtime.
knownLength :: forall n a r.
               Vector n a -- ^ a vector of some (potentially unknown) length
            -> (KnownNat n => r) -- ^ a value that depends on knowing the vector's length
            -> r -- ^ the value computed with the length
knownLength = V.knownLength

-- | /O(1)/ Reveal a 'KnownNat' instance and 'Proxy' for a vector's length,
-- determined at runtime.
knownLength' :: forall n a r.
                Vector n a -- ^ a vector of some (potentially unknown) length
             -> (KnownNat n => Proxy n -> r) -- ^ a value that depends on knowing the vector's length, which is given as a 'Proxy'
             -> r -- ^ the value computed with the length
knownLength' = V.knownLength'

-- | /O(1)/ Reveal a 'KnownNat' instance for a vector's length, determined
-- at runtime.
knownLength :: forall v n a r. VG.Vector v a
            => Vector v n a -- ^ a vector of some (potentially unknown) length
            -> (KnownNat n => r) -- ^ a value that depends on knowing the vector's length
            -> r -- ^ the value computed with the length
knownLength v x = knownLength' v $ const x

-- | /O(1)/ Reveal a 'KnownNat' instance and 'Proxy' for a vector's length,
-- determined at runtime.
knownLength' :: forall v n a r. VG.Vector v a
             => Vector v n a -- ^ a vector of some (potentially unknown) length
             -> (KnownNat n => Proxy n -> r) -- ^ a value that depends on knowing the vector's length, which is given as a 'Proxy'
             -> r -- ^ the value computed with the length
knownLength' (Vector v) x = case someNatVal (fromIntegral (VG.length v)) of
  Just (SomeNat (Proxy :: Proxy n')) -> case unsafeCoerce Refl :: n' :~: n of Refl -> x Proxy
  Nothing -> error "impossible: Vector has negative length"



------ access
https://stackoverflow.com/questions/35146444/tensorflow-python-accessing-individual-elements-in-a-tensor

-- eigen
modify :: (Elem a, KnownNat n, KnownNat m) => (forall s. MMatrix n m s a -> ST s ()) -> Matrix n m a -> Matrix n m a




-- | Retrieve a single row from a matrix
--
-- FIXME: Use 'Idx' and remove the 'throwString' function
getRow
  :: forall t n m . (All KnownDim '[n, m], KnownNat m)
  => Tensor '[n, m] -> Word -> Maybe (Tensor '[1, m])
getRow t r
  | r > fromDim (dim :: Dim n) = Nothing
  | otherwise = unsafeDupablePerformIO $ do

-- | Retrieve a single column from a matrix
--
-- FIXME: Use 'Idx' and remove the 'throwString' function
getColumn
  :: forall t n m . (All KnownDim '[n, m], KnownNat n)
  => Tensor '[n, m] -> Word -> Maybe (Tensor '[n, 1])
getColumn t r
  | r > fromDim (dim :: Dim m) = Nothing
  | otherwise = unsafeDupablePerformIO $ do





http://hackage.haskell.org/package/finite-typelits-0.1.4.2/docs/Data-Finite-Internal.html#t:Finite

-- | /O(1)/ Safe indexing using a 'Finite'.
index :: forall n a. ()
      => Vector n a -> Finite n -> a
index = V.index
{-# inline index #-}

-- | /O(1)/ Safe indexing using a 'Proxy'.
index' :: forall n m a p. KnownNat n
       => Vector (n+m+1) a -> p n -> a
index' = V.index'
{-# inline index' #-}


-- | /O(1)/ Safe indexing using a 'Finite'.
index :: forall v n a. VG.Vector v a
      => Vector v n a -> Finite n -> a
index (Vector v) (Finite i) = v `VG.unsafeIndex` fromIntegral i
{-# inline index #-}

-- | /O(1)/ Safe indexing using a 'Proxy'.
index' :: forall v n m a p. (KnownNat n, VG.Vector v a)
       => Vector v (n+m+1) a -> p n -> a
index' (Vector v) p = v `VG.unsafeIndex` i
  where i = fromIntegral (natVal p)
{-# inline index' #-}


http://hackage.haskell.org/package/linear-1.20.8/docs/src/Linear.Metric.html#Metric
class Additive f => Metric f where
  -- | Compute the inner product of two vectors or (equivalently)
  -- convert a vector @f a@ into a covector @f a -> a@.
  --
  -- >>> V2 1 2 `dot` V2 3 4
  -- 11
  dot :: Num a => f a -> f a -> a
#ifndef HLINT
  default dot :: (Foldable f, Num a) => f a -> f a -> a
  dot x y = Foldable.sum $ liftI2 (*) x y
#endif

  -- | Compute the squared norm. The name quadrance arises from
  -- Norman J. Wildberger's rational trigonometry.
  quadrance :: Num a => f a -> a
  quadrance v = dot v v

  -- | Compute the quadrance of the difference
  qd :: Num a => f a -> f a -> a
  qd f g = quadrance (f ^-^ g)

  -- | Compute the distance between two vectors in a metric space
  distance :: Floating a => f a -> f a -> a
  distance f g = norm (f ^-^ g)

  -- | Compute the norm of a vector in a metric space
  norm :: Floating a => f a -> a
  norm v = sqrt (quadrance v)

  -- | Convert a non-zero vector to unit vector.
  signorm :: Floating a => f a -> f a
  signorm v = fmap (/m) v where
    m = norm v

instance Dim n => Metric (V n) where
  dot (V a) (V b) = V.sum $ V.zipWith (*) a b





class Boolean where
  and &&&
  or  |||
  not
  xor 


--** operator typeclasses

create abstract operator typeclasses, use this as a guide:
- https://github.com/mstksg/hmatrix-backprop/blob/master/src/Numeric/LinearAlgebra/Static/Backprop.hs
- https://github.com/hasktorch/hasktorch/blob/master/indef/tests/Torch/Indef/Static/TensorSpec.hs

- (w/o IndexTensor) https://github.com/hasktorch/hasktorch/blob/master/indef/src/Torch/Indef/Index.hs
- https://github.com/hasktorch/hasktorch/blob/master/indef/src/Torch/Indef/Static/Tensor/Index.hs
- https://github.com/hasktorch/hasktorch/blob/master/indef/src/Torch/Indef/Static/Tensor.hs
- https://github.com/hasktorch/hasktorch/blob/master/indef/src/Torch/Indef/Static/Tensor/Math.hs
- https://github.com/hasktorch/hasktorch/blob/master/indef/src/Torch/Indef/Static/Tensor/Math/Reduce/Floating.hs https://github.com/hasktorch/hasktorch/blob/master/indef/src/Torch/Indef/Static/Tensor/Math/Reduce.hs

https://github.com/hasktorch/hasktorch/blob/master/indef/src/Torch/Indef/Static/Tensor/Math/Pairwise.hs
https://github.com/hasktorch/hasktorch/blob/master/indef/src/Torch/Indef/Static/Tensor/Math/Pointwise.hs
https://github.com/hasktorch/hasktorch/blob/master/indef/src/Torch/Indef/Static/Tensor/Math/Blas.hs






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


softmax
  :: KnownDim n
  => Reifies s W
  => BVar s (Tensor '[n])    -- ^ input
  -> BVar s (Tensor '[n])    -- ^ output
softmax = softmaxN (dim :: Dim 0)





  -- contract 


-- | infix 'matmul'

infixl 7 #,.#,#.,<#,#>,<#>

(#) :: T t (i ': j ': x) e -> T t (j ': k ': x) e -> T t (i ': k ': x) e 
(#) = matmul

(<#>)
dot :: KnownShape s, Num e => T t s e -> T t s e -> e
dot a b = reduce $ a `mul` b

(#>) :: (All KnownDim '[r, c]) => Tensor '[r, c] -> Tensor '[c] -> Tensor '[r]
(#>) a b = mv a b



class (Tensor t, Shape s, Elt e) => Tensile (t s e) where

  -- s1234 have optic-like relationship, 
  -- easy to impl w/ Fuctor/Foldable w/ Index
  tmap :: (t s1 a -> t s2 a) -> t s3 a -> t s4 a
  https://github.com/AccelerateHS/accelerate/blob/dc743a12753f3cd22c6f5184576112ed801e682b/src/Data/Array/Accelerate/Data/Functor.hs#L48
  http://hackage.haskell.org/package/linear-accelerate-0.6.0.0/docs/src/Data-Array-Accelerate-Linear-V1.html#line-154
  tmap :: (t n a -> t s2 a) -> t s3 a -> t s4 a





# If x is complex, setting conjugate=True gives the conjugate transpose

# 'perm' is more useful for n-dimensional tensors, for n > 2
x = tf.constant([[[ 1,  2,  3],
                  [ 4,  5,  6]],
                 [[ 7,  8,  9],
                  [10, 11, 12]]])
shape = [2,2,3]
--https://www.tensorflow.org/api_docs/python/tf/transpose
# Take the transpose of the matrices in dimension-0
# (this common operation has a shorthand `linalg.transpose`)
tf.transpose(x, perm=[0, 2, 1])  # [[[1,  4],
                                 #   [2,  5],
                                 #   [3,  6]],
                                 #  [[7, 10],
                                 #   [8, 11],
                                 #   [9, 12]]]
shape = [2,3,2]




