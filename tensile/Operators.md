https://github.com/helq/tensorflow-haskell-deptyped/blob/master/library/TensorFlow/DepTyped/Ops.hs

https://www.tensorflow.org/versions/r2.0/api_docs/python/tf
https://pytorch.org/docs/stable/tensors.html

Kolda et al - Tensor Decompositions and Applications
http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=4A0663C7848627DADDBA6A243BC43E78?doi=10.1.1.130.782&rep=rep1&type=pdf

compat
config
data
debugging
distribute
distributions
dtypes
errors
estimator
feature_column
graph_util
image
initializers
io
keras
layers
logging
losses
manip
matrix (tf.linalg)
metrics
nn
quantization
queue
ragged
random
reduce (ai.onnx reduce*)
resource_loader
rnn
saved_model
sets
signal
sparse
strings
summary
sysconfig
test --
train -- put code here

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
      DiagAbove -> fromIntegral $ dimVal offset
      DiagBelow -> -(fromIntegral $ dimVal offset)

-- | Create a diagonal matrix from a 1D vector
diag1d :: (KnownDim n) => Tensor '[n] -> Tensor '[n, n]
diag1d t = diagUnsafe t 0




----- shape manipulation

-- | Static call to 'Dynamic.cat'
cat
  :: '(ls, r0:+rs) ~ Sing.SplitAt i d
  => '(ls, r1:+rs) ~ Sing.SplitAt i d'
  => Tensor d
  -> Tensor d'
  -> Dim (i::Nat)
  -> Tensor (ls ++ '[r0 + r1] ++ rs)
cat a b d = fromRight (error "impossible: cat type should not allow this branch") $
  asStatic <$> Dynamic.cat (asDynamic a) (asDynamic b) (fromIntegral $ dimVal d)

-- | convenience function, specifying a type-safe 'cat' operation.
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

-- | convenience function, specifying a type-safe 'cat' operation.
cat2d1 :: (All KnownDim '[n,m,m0,m1], m ~ Sing.Sum [m0, m1]) => Tensor '[n, m0] -> Tensor '[n, m1] -> (Tensor '[n, m])
cat2d1 a b = cat a b (dim :: Dim 1)

-- | convenience function, stack two rank-1 tensors along the 1-dimension
stack1d1 :: KnownDim n => Tensor '[n] -> Tensor '[n] -> (Tensor '[n, 2])
stack1d1 a b = cat2d1
  (unsqueeze1d (dim :: Dim 1) a)
  (unsqueeze1d (dim :: Dim 1) b)


-- tf dep typed
reshape :: forall t shape1 shape2 phs v.
           (TF.TensorType t, ShapeProduct shape1 ~ ShapeProduct shape2, KnownNatList shape2)
        => Tensor shape1 phs v t
        -> Tensor shape2 phs Build t
reshape (Tensor t) = Tensor $ TF.reshape t $ TF.vector $ map
            (fromInteger :: Integer -> Int32)
            (natListVal (Proxy :: Proxy shape2))


-- WARNING: This might be not be garbage collected as you expect since the input argument becomes a dangling phantom type.
reshape :: forall d d' . (All Dimensions [d,d'], Product d ~ Product d') => Tensor d -> Tensor d'
reshape src = unsafeDupablePerformIO $
  resizeAsT_ (newClone src :: Tensor d) (new :: Tensor d')
{-# NOINLINE resizeAs #-}

-- | flatten a tensor (pure, dupable)
flatten :: (Dimensions d, KnownDim (Product d)) => Tensor d -> Tensor '[Product d]
flatten = resizeAs

unsqueeze1d
  :: Dimensions d
  => '(rs, ls) ~ (SplitAt n d)
  => Dim n
  -> Tensor d
  -> Tensor (rs ++ '[1] ++ ls)

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


-- | Select a dimension of a tensor. If a vector is passed in, return a singleton tensor
-- with the index value of the vector.
slice
  :: forall d ls r rs i
  .  '(ls, r:+rs) ~ SplitAt i d
  => KnownDim i
  => Dimensions d
  => Tensor d
  -> Dim i
  -> Tensor (ls ++ rs)
slice t i = unsafePerformIO $

(!!) = slice -- hasktorch

-- | Retrieve a single row from a matrix
--
-- FIXME: Use 'Idx' and remove the 'throwString' function
getRow
  :: forall t n m . (All KnownDim '[n, m], KnownNat m)
  => Tensor '[n, m] -> Word -> Maybe (Tensor '[1, m])
getRow t r
  | r > dimVal (dim :: Dim n) = Nothing
  | otherwise = unsafeDupablePerformIO $ do

-- | Retrieve a single column from a matrix
--
-- FIXME: Use 'Idx' and remove the 'throwString' function
getColumn
  :: forall t n m . (All KnownDim '[n, m], KnownNat n)
  => Tensor '[n, m] -> Word -> Maybe (Tensor '[n, 1])
getColumn t r
  | r > dimVal (dim :: Dim m) = Nothing
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


signature Reduce where

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


softmax
  :: KnownDim n
  => Reifies s W
  => BVar s (Tensor '[n])    -- ^ input
  -> BVar s (Tensor '[n])    -- ^ output
softmax = softmaxN (dim :: Dim 0)


  shape :: (KnownShape s, Len s ~ n, Elt e) => T t s e -> V n Int
  shape = 

  constant :: (KnownShape s, Len s ~ n, Size s ~ n', Elt e) => V n' e -> T t s e
  -- constant v = fill $ const v

  -- forces a denotation of data ordering (e.g. row-major, col-major etc)
  fill :: Monad m => (Idxs s -> e) -> m (T t s e)


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

  -- like TF.pack (python tf.stack) 
  -- https://github.com/onnx/onnx/blob/master/docs/Operators.md#concat
  concat :: V n (t s a) -> t (n:s) a


  -- https://www.tensorflow.org/xla/broadcasting
  -- https://github.com/onnx/onnx/blob/master/docs/Broadcasting.md
  -- Need a recursive typelevel function for Comp
  -- https://github.com/onnx/onnx/blob/master/docs/Operators.md#Expand
  bcast (Comp s s') :: t s e -> t s' e



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




