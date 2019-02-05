{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UnboxedTuples          #-}
{-# LANGUAGE UndecidableInstances, RankNTypes, AllowAmbiguousTypes, PolyKinds  #-}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures#-} 
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}

module Numeric.Tensile.Operations.Linear.Internal where

import Data.Tensor.Types
import Data.Tensor.Internal.Array
import Data.Vector.Storable (Vector(..), Storable(..))
import Control.Monad.ST (ST(..))
import Numeric.Dimensions (Nat, TypedList(..), Dimensions(..), Idx(..), Idxs(..), Dims(..), Dim(..), KnownDim(..), Reverse, Length)
import Numeric.Dim

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M
import qualified Numeric.Dimensions as D hiding (idxsFromWords) -- broken function

import Numeric.Type.Evidence
import qualified Numeric.TypedList as T

import Data.Monoid
import Data.Word

---------------- Data.Dimensions

-- import GHC.TypeNats
import qualified Math.Combinat.Permutations as P

-- Numeric.Tensile.Dimensions
--

-- import Data.Reflection hiding (KnownNat(..))
import Data.Singletons.Prelude.List (Sort(..))
import Data.Proxy
import Unsafe.Coerce (unsafeCoerce)

type family Rank (xs :: [k]) :: Nat where
    Rank '[] = 0
    Rank (_ ': xs) = 1 + Rank xs

type family Size (xs :: [k]) :: Nat where
    Size '[] = 1
    Size (x ': xs) = x * Size xs

type Permutable ds ds' = (Sort ds ~ Sort ds')
type Reshapable ds ds' = (Size ds ~ Size ds')

class Reifies s a | s -> a where
  -- | Recover a value inside a 'reify' context, given a proxy for its
  -- reified type.
  reflect :: proxy s -> a

instance Dimensions (ds :: [Nat]) => Reifies ds (Dims ds) where
  reflect _ = dims

newtype MagicDims r = MagicDims (forall (ds :: [Nat]). Dimensions ds => Proxy ds -> r)

reifyDims :: forall r. [Word] -> (forall (ds :: [Nat]). Dimensions ds => Proxy ds -> r) -> r
reifyDims ds k = unsafeCoerce (MagicDims k :: MagicDims r) ds Proxy

instance KnownDim (d :: Nat) => Reifies d (Dim d) where
  reflect _ = dim

newtype MagicDim r = MagicDim (forall (d :: Nat). KnownDim d => Proxy d -> r)

reifyDim :: forall r. Word -> (forall (d :: Nat). KnownDim d => Proxy d -> r) -> r
reifyDim d k = unsafeCoerce (MagicDim k :: MagicDim r) d Proxy

-- | Remaps the index argument to the index with the same 'Int' representation under the permuted dimensions.
remapIdxs 
  :: forall r (ds :: [Nat]). Perm (Rank ds) 
  -> Dims ds 
  -> Idxs ds 
  -> (forall (ds' :: [Nat]). Dims ds' -> Idxs ds' -> r) 
  -> r
remapIdxs (Perm p) ds ix f = 
  reifyDims (P.permuteList p $ D.listDims ds) $ \ds' -> 
    f (reflect ds') (toIdxs (reflect ds') . fromIdxs ds $ ix)




{-
 -
-- test -

--reify :: a -> (forall s. Reifies s a => Proxy s -> r) -> r

reify d233 $ \p -> D.totalDim' $ reflect p

reifyDims (reverse [2,3,3]) $ \p -> overDimIdx_ (reflect p) print
> reifyDims (reverse [2,3,3]) $ \p -> D.overDimIdx_ (reflect p) print
Idxs [1,1,1]
Idxs [2,1,1]
Idxs [3,1,1]
Idxs [1,2,1]
Idxs [2,2,1]
Idxs [3,2,1]
Idxs [1,3,1]
Idxs [2,3,1]
Idxs [3,3,1]
Idxs [1,1,2]
Idxs [2,1,2]
Idxs [3,1,2]
Idxs [1,2,2]
Idxs [2,2,2]
Idxs [3,2,2]
Idxs [1,3,2]
Idxs [2,3,2]
Idxs [3,3,2]

> reifyDims (reverse [2,3,3]) $ \p -> Numeric.Tensile.Operations.Linear.Internal.overDimIdx_ (reflect p) print
Idxs [1,1,1]
Idxs [1,1,2]
Idxs [1,2,1]
Idxs [1,2,2]
Idxs [1,3,1]
Idxs [1,3,2]
Idxs [2,1,1]
Idxs [2,1,2]
Idxs [2,2,1]
Idxs [2,2,2]
Idxs [2,3,1]
Idxs [2,3,2]
Idxs [3,1,1]
Idxs [3,1,2]
Idxs [3,2,1]
Idxs [3,2,2]
Idxs [3,3,1]
Idxs [3,3,2]

-- test -

re :: Perm (Rank '[2, 3, 3])
re = reversal

a :: Idxs '[2, 3, 3]
a = fromJust $ idxsFromWords [2, 1, 3]  

> D.overDimIdx_ (dims @_ @'[2,3,3]) print
Idxs [1,1,1]
Idxs [2,1,1]
Idxs [1,2,1]
Idxs [2,2,1]
Idxs [1,3,1]
Idxs [2,3,1]
Idxs [1,1,2]
Idxs [2,1,2]
Idxs [1,2,2]
Idxs [2,2,2]
Idxs [1,3,2]
Idxs [2,3,2]
Idxs [1,1,3]
Idxs [2,1,3]
Idxs [1,2,3]
Idxs [2,2,3]
Idxs [1,3,3]
Idxs [2,3,3]

> D.overDimIdx_ (dims @_ @'[2,3,3]) (\i -> remapIdxs re (dims @_ @'[2,3,3]) i print)
Idxs [1,1,1]
Idxs [2,1,1]
Idxs [3,1,1]
Idxs [1,2,1]
Idxs [2,2,1]
Idxs [3,2,1]
Idxs [1,3,1]
Idxs [2,3,1]
Idxs [3,3,1]
Idxs [1,1,2]
Idxs [2,1,2]
Idxs [3,1,2]
Idxs [1,2,2]
Idxs [2,2,2]
Idxs [3,2,2]
Idxs [1,3,2]
Idxs [2,3,2]
Idxs [3,3,2]

D.overDimIdx_ f print
D.overDimIdx_ f (\i -> remapIdxs re f i (\_ j -> print j))



-}

majorToMinor :: forall ds i. Integral i => Dims ds -> Idxs ds -> i
majorToMinor dims = fromIntegral . go 1 dims
  where
    go :: forall ns . Word -> Dims ns -> Idxs ns -> Word
    go _ U U                     = 0
    go m (d :* ds) (Idx i :* is) = m * (i - 1) + go (m * D.dimVal d) ds is

minorToMajor :: forall ds i. Integral i => Dims ds -> Idxs ds -> i
minorToMajor d i = majorToMinor (reversed d) (reversed i)

fromIdxs :: forall ds i. Integral i => Dims ds -> Idxs ds -> i
fromIdxs = minorToMajor

toIdxs :: forall ds i. Integral i => Dims ds -> i -> Idxs ds
toIdxs dsd i = go dsd $ fromIntegral i
  where
    go :: forall ns . Dims ns -> Word -> Idxs ns
    go U 0 = U
    go U _ = error ("Idxs " ++ show (D.listDims dsd))
    go (T.Snoc ds d) off = case divMod off (D.dimVal d) of
      (off', j) -> go ds off' `T.snoc` Idx (j+1)





{-
toIdxs :: forall ds i. Integral i => Dims ds -> i -> Idxs ds
toIdxs dds i = go dds $ fromIntegral i
  where
    go :: forall ns . Dims ns -> Word -> Idxs ns
    go U 0 = U
    go U _ = error ("Idxs " ++ show (D.listDims dds))
    go (d :* ds) off = case divMod off (D.dimVal d) of
      (off', j) -> Idx (j+1) :* go ds off'


--TODO why wont this compile
fromIdxs :: Dims ds -> Idxs ds -> Int
fromIdxs dsd = fromIntegral . go 1 dsd
  where
    go :: forall ns . Word -> Dims ns -> Idxs ns -> Word
    go _ U U                     = 0
    go m (T.Snoc ds d) (T.Snoc is (Idx i)) = m * (i - 1) + go (m * D.dimVal d) ds is
-}

idxsFromWords :: forall ds . Dimensions ds => [Word] -> Maybe (Idxs ds)
idxsFromWords = unsafeCoerce . go (D.listDims (D.dims @_ @ds))
  where
    go [] [] = Just []
    go (d : ds) (i : is)
      | i > 0 && i <= d = (i:) <$> go ds is
    go _ _   = Nothing

foldDimPartIdx :: Idxs ds -- ^ Initial indices
               -> Idxs ds -- ^ Final indices
               -> (Idxs ds -> a -> a)
                          -- ^ Function to call on each dimension
               -> a       -- ^ initial value
               -> a
foldDimPartIdx U U k = k U
foldDimPartIdx (start :* starts) (end :* ends) k
  | iEnd >= iStart = foldDimPartIdx starts ends (loop iStart)
  | otherwise      = foldDimPartIdx starts ends (looi iStart)
  where
    Idx iStart = start
    Idx iEnd   = end
    loop i is
      | i > iEnd = id
      | otherwise = k (Idx i :* is) . loop (i+1) is
    looi i is
      | i < iEnd = id
      | otherwise = k (Idx i :* is) . looi (i-1) is

overDimPartIdx_ :: Monad m
               => Idxs ds -- ^ Initial indices
               -> Idxs ds -- ^ Final indices
               -> (Idxs ds -> m ())
                          -- ^ Function to call on each dimension
               -> m ()
overDimPartIdx_ U U k = k U
overDimPartIdx_ (start :* starts) (end :* ends) k
  | iEnd >= iStart = overDimPartIdx_ starts ends loop'
  | otherwise      = overDimPartIdx_ starts ends looi'
  where
    Idx iStart = start
    Idx iEnd   = end
    loop' is = loop iStart
      where
        loop i
          | i > iEnd = return ()
          | otherwise = k (Idx i :* is) >> loop (i+1)
    looi' is = looi iStart
      where
        looi i
          | i < iEnd = return ()
          | otherwise = k (Idx i :* is) >> looi (i-1)


overDimIdx_ :: Monad m
            => Dims ds -- ^ Shape of a space
            -> (Idxs ds -> m ()) -- ^ Function to call on each dimension
            -> m ()
overDimIdx_ U k = k U
overDimIdx_ (T.Snoc ds d) k = overDimIdx_ ds k'
  where
    dw = D.dimVal d
    k' is = go 1
      where
        go i
          | i > dw = return ()
          | otherwise = k (is `T.snoc` Idx i) >> go (i+1)


-----------------------

-- Numeric.Tensile.Permutation
instance Semigroup (Perm n) where
  (Perm p) <> (Perm q) = Perm $ P.multiply p q

instance KnownDim n => Monoid (Perm n) where
  mempty = Perm $ P.identity (fromIntegral $ D.dimVal' @n)

newtype Perm (n :: Nat) = Perm { unPerm :: P.Permutation } deriving (Eq, Ord, Show)


cycles (Perm t) = P.permutationToDisjointCycles $ t

-- | Transform a permutation of tensor modes into a permutation of array indices.
-- transpose (lowerPerm p1) . transpose (lowerPerm p2) == transpose (lowerPerm $ p1 <> p2)
lowerPerm 
  :: forall ds. KnownDim (Size ds) 
  => Dims ds 
  -> Perm (Rank ds) -- ^ Rank-level permutation
  -> (Dims ds -> Idxs ds -> Perm (Rank ds) -> Perm (Size ds)) -- ^ Index filter
  -> Perm (Size ds)  -- ^ Index-level permutation
lowerPerm d p f = D.foldDimIdx d (\i p' -> p' <> f d i p) (mempty :: Perm (Size ds))

{-
lowerPerm'
  :: forall ds. KnownDim (Size ds) 
  => (Dims ds' -> Idxs ds' -> Perm (Rank ds) -> Perm (Rank ds))
  -> Dims ds 
  -> Perm (Rank ds) -- ^ Rank-level permutation
  -> Perm (Rank ds)  -- ^ Index-level permutation
lowerPerm' d p f = D.foldDimIdx d (\i p' -> p' <> f d i p) (mempty :: Perm (Size ds))
-}
-- f :: (Dims ds' -> Perm n -> Perm n) -> Perm n -> Tensor t ds -> Tensor t ds'
-- f dim2Idx perm t = Tensor $ reifyDims (permuteDims perm (dims @_ @ds)) $ \p ->
--   modifyIdx (reflect p) (modify (permuteIdxs (dim2Idx (reflect p) perm) _)) (reflect p)) t -- basically make user derive the Idxs ds' -> Idxs ds'

-- dim2Idx :: Rank ds ~ n => Dims ds -> Perm n -> Perm n
-- takes a perm on dimensions and derives a perm in indices, eg
-- dim2Idx d p = lowerPerm' ...
-- --
-- otherwise consider using the raw index folds and lowerPerm???
-- could also create :  Perm ds ds'

-- minorToMajor = transpose (lowerPerm reversal)
-- see https://www.tensorflow.org/xla/shapes#minor-to-major_dimension_ordering


--------------------------------------
--


permuted :: Perm (Rank ds) -> TypedList f ds -> TypedList f ds'
permuted (Perm p) = unsafeCoerce . P.permuteList p . unsafeCoerce

reversed :: TypedList f ds -> TypedList f ds'
reversed = unsafeCoerce . reverse . unsafeCoerce 

reversal :: forall n. KnownDim n => Perm n
reversal = Perm $ P.reversePermutation n
  where n = fromIntegral $ D.dimVal' @n 

reversal' :: Word -> Perm n
reversal' n = Perm $ P.reversePermutation (fromIntegral n)

transposition' :: forall n. KnownDim n => Word -> Word -> Maybe (Perm n)
transposition' i j = if i <= n' && j <= n' then Just p else Nothing
  where
    p = Perm $ P.transposition n (fromIntegral i, fromIntegral j)
    n = fromIntegral $ D.dimVal' @n 
    n' = fromIntegral n

{-
transposition'' :: Word -> Word -> Word -> Maybe (Perm n)
transposition'' n i j = 
  reifyDim n $ \n ->
    reifyDim i $ \i -> 
      reifyDim j $ \j -> transposition' (reflect i) (reflect j)

-- TODO clean up type sig
transposition
  :: forall i j n. (i <= n, j <= n)
  => (KnownDim i, KnownDim j, KnownDim n) --All KnownDim [i,j,n]
  => Perm n
transposition = Perm $ P.transposition n (i,j)
  where
    n = fromIntegral $ D.dimVal' @n 
    i = fromIntegral $ D.dimVal' @i 
    j = fromIntegral $ D.dimVal' @j 

--ttt :: Perm 3
--ttt = transposition @2 @3

-}



reshape 
  :: forall t ds ds'. Elt t 
  => Reshapable ds ds'
  => Tensor t ds -> Tensor t ds'
reshape = unsafeCoerce

transpose 
  :: forall t ds ds'. Elt t 
  => Dimensions ds
  => Permutable ds ds'
  => Perm (Rank ds) -> Tensor t ds -> Tensor t ds'
transpose p (Tensor v) = Tensor v'
  where
    d = D.dims @_ @ds
    v' = modifyIdxs d v $ \i m -> 
           remapIdxs p d i $ \d' i' -> 
             M.modify m (const $ v V.! fromIdxs d' (permuted p i)) (fromIdxs d' i')


fromScalar :: Elt t => t -> Tensor t '[]
fromScalar = constant (D.dims @_ @'[])

constant :: Elt t => Dims ds -> t -> Tensor t ds
constant dims t = fill dims $ const t

fill :: forall t ds. Elt t => Dims ds -> (Idxs ds -> t) -> Tensor t ds
fill dims f = Tensor $ V.create $ do
  mv <- M.new (fromIntegral $ D.totalDim dims)
  let act idxs = M.write mv (minorToMajor dims idxs) $ f idxs
  overDimIdx_ dims act
  return mv

{-

fill :: Elt t => Dims ds -> (Int -> t) -> Vector t
fill dims act = 
  case dims of 
    D.Reverse dims' -> fill' dims act
   
fill' :: Elt t => Dims ds -> (Int -> t) -> Vector t
fill' dims act = V.create $ do
  v <- M.new (fromIntegral $ D.totalDim dims)
  let f i = M.write v i $ act i
  D.overDimOff_ dims f 0 1 -- either control the offset ourselves or fix type issues
  return v

TODO add tests:
> mod ds idxs m = M.swap m 0 (fromIdxs ds idxs)
> v = V.fromList [1..12]
> v
[1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0]
> modifyIdxs (dims @_ @'[2, 2, 3]) v $ mod (dims @_ @'[2, 2, 3])
[12.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0]

> modifyIdxs (dims @_ @'[2, 2, 3]) v (\_ m -> M.set m 0)
[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]

-}


modifyIdxs :: forall t ds. Storable t => Dims ds -> Vector t -> (forall s. Idxs ds -> M.MVector s t -> ST s ()) -> Vector t
modifyIdxs dims v f = V.modify (\mv -> overDimIdx_ dims (\i -> f i mv)) v




{- unit tests
 -
 -

d233 = (dims @_ @'[2,3,3])
d332 = (dims @_ @'[3,3,2])

ttt :: Perm 3
ttt = transposition @2 @3

w = fill d233 $ majorToMinor d233 :: Vector Int -- col major
w' = fill d233 $ minorToMajor d233 :: Vector Int -- row major



--test - transposing a matrix
--
re :: Perm (Rank '[2, 4])
re = reversal

d24 = (dims @_ @'[2, 4])
d42 = (dims @_ @'[4, 2])

t :: Tensor Int '[2, 4]
t = fill d24 $ majorToMinor d24 -- col major

t' :: Tensor Int '[2, 4] 
t' = fill d24 $ minorToMajor d24 -- row major

t'' :: Tensor Int '[4, 2] 
t'' = fill d42 $ minorToMajor d42 -- row major

res = reshape t :: Tensor Int '[4, 2]
res' = transpose re t' :: Tensor Int '[4, 2]

transpose t' == reshape t && transpose t == reshape t'

--test - reversing a cube

re :: Perm (Rank '[3, 3, 3])
re = reversal

d333 = (dims @_ @'[3,3,3])

t :: Tensor Int '[3, 3, 3] 
t = fill d333 $ majorToMinor d333 -- col major

t' :: Tensor Int '[3, 3, 3] 
t' = fill d333 $ minorToMajor d333 -- row major

res :: Tensor Int '[3, 3, 3] 
res = transpose re t'

transpose re t' == reshape t && transpose re t == reshape t'



--test 2:

res = modifyIdx d233 (modify (filterIdx triangular'' ttt) d233) w'
res == [1,4,7,2,5,8,3,6,9,10,13,16,11,14,17,12,15,18]


--test 
a :: Maybe (Idxs '[2, 3, 3])
a = idxsFromWords [2, 1, 1]  

b :: Maybe (Idxs '[2, 3, 3])
b = idxsFromWords [2, 3, 3] 

check :: Dims ds -> Idxs ds -> Idxs ds -> Perm (Rank ds) -> (Perm (Rank ds) -> Idxs ds -> Idxs ds) -> [(Int, Int)]
check d i j p f = foldDimPartIdx i j acc []
  where 
    acc i l = if (i/=f p i) then (1 + minorToMajor d i, 1 + minorToMajor d (f p i)) : l else l

res = liftM2 (\i j -> check d233 i j ttt $ filterIdx triangular'') a b

res == Just [(11,13),(12,16),(15,17)]


-}




{-

gen# 
  :: forall s t ds. PrimBytes t 
  => Int# -- ^ number of elements, not checked!
             --   Avoid using this argument if possible.
  -> (s -> (# s, t #))
  -> s -> (# s, ArrayBase t ds #)
gen# n f z0 = go (byteSize @t undefined *# n)
  where
    go bsize = case runRW#
     ( \s0 -> case newByteArray# bsize s0 of
         (# s1, mba #) -> case loop0 mba 0# z0 s1 of
           (# s2, z1 #) -> case unsafeFreezeByteArray# mba s2 of
             (# s3, ba #) -> (# s3, (# z1, ba #) #)
     ) of (# _, (# z1, ba #) #) -> (# z1, ArrayBase (# | (# 0# , n , ba #) #) #)
    {-# NOINLINE go #-}
    loop0 mba i z s
      | isTrue# (i ==# n) = (# s, z #)
      | otherwise = case f z of
          (# z', x #) -> loop0 mba (i +# 1#) z' (writeArray mba i x s)

3-D tensor `a` w shape=[2, 2, 3]
[[[ 1,  2,  3],
  [ 4,  5,  6]],
 [[ 7,  8,  9],
  [10, 11, 12]]]


3-D tensor `b` w shape=[2, 3, 2]
[[[13, 14],
  [15, 16],
  [17, 18]],
 [[19, 20],
  [21, 22],
  [23, 24]]]
                )

mul `a` `b` has shape=[2,2,2]
[[[ 94, 100],
  [229, 244]],
 [[508, 532],
  [697, 730]]]
-}


{-
product 
  :: forall m x y. KnownDim m
  => Dimensions x
  => Dimensions y
  => T (x +: m) -> T (m :+ y) -> T (x ++ y)
product t u
    | I# m <- fromIntegral $ dimVal' @m
    , I# n <- fromIntegral $ totalDim' @x
    , I# k <- fromIntegral $ totalDim' @y
    , nk <- n *# k
    = let loop1 i j l r | isTrue# (l ==# m) = r
                        | otherwise = loop1 i j (l +# 1#)
                          (r + ix# (i +# n *# l) t * ix# (l +# m *# j) u)

          loop2 (T# i j) | isTrue# (j ==# k) = (# T# i j, 0 #)
                         | isTrue# (i ==# n) = loop2 (T# 0# (j +# 1#))
                         | otherwise = (# T# (i +# 1#) j, loop1 i j 0# 0 #)
      in case gen# nk loop2 (T# 0# 0#) of
          (# _, r #) -> r

data T# = T# Int# Int#

-- mode-i tensor-matrix product
-- see http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=4A0663C7848627DADDBA6A243BC43E78?doi=10.1.1.130.782&rep=rep1&type=pdf
product
  :: forall m x y. KnownDim m
  => Dimensions x
  => Dimensions y
  => T (x ++ [m] ++ y) 
  -> T '[m, n]
  -> T (x ++ n ++ y)

-- #>
productR
  :: All KnownDim '[a, b, c]
  => Dimensions x
  => T (a :+ b :+ x)
  -> T (b :+ c :+ x)
  -> T (a :+ c :+ x)
productR = undefined

-- <#
-- same as tf.matmul
productN
  :: All KnownDim '[a, b, c]
  => Dimensions x
  => T (x +: a +: b) 
  -> T (x +: b +: c)
  -> T (x +: a +: c)
productN = undefined


transpose
  :: forall m n x. All KnownDim '[m, n]
  => T '[n, m] --(n :+ m :+ x) 
  -> T '[m, n] --(m :+ n :+ x)
transpose t = case elemSize0 t of
  0# -> broadcast (ix# 0# t)
  nm | I# m <- fromIntegral $ dimVal' @m
     , I# n <- fromIntegral $ dimVal' @n
     -> let f ( I# i,  I# j )
              | isTrue# (i ==# m) = f ( 0 , I# (j +# 1#) ) -- skip to next col
              | otherwise         = (# ( I# (i +# 1#), I# j ), ix# (i *# n +# j) t #) --col-major indexing
        in case gen# nm f (0,0) of
          (# _, r #) -> r


-}


