module Numeric.Tensile.Index (
  module Numeric.Dimensions.Idxs,
  module Numeric.Tensile.Index 
) where

import Numeric.Dimensions.Idxs (Idx(..), Idxs(..))
import Numeric.Tensile.Types (Nat, TypedList(..), Dims(..), Dim(..), KnownDim(..), KnownDims(..), Size, Rank)
import Numeric.Tensile.Permutation
import Unsafe.Coerce (unsafeCoerce)

import qualified Math.Combinat.Permutations as P
import qualified Numeric.Dimensions.Idxs as I
import qualified Numeric.Dimensions.Fold as D 
import qualified Numeric.Tensile.Types as T

-- (Idx(),Idxs(),listIdxs,idxToWord,idxFromWord,unsafeIdxFromWord)

_permuted :: Perm (Rank ds) -> TypedList f ds -> TypedList f ds'
_permuted (Perm p) = unsafeCoerce . P.permuteList p . unsafeCoerce

_reversed :: TypedList f ds -> TypedList f ds'
_reversed = unsafeCoerce . reverse . unsafeCoerce 

-- | Remaps the index argument to the index with the same 'Int' representation under the permuted dimensions.
remapIdxs 
  :: forall r (ds :: [Nat]). Perm (Rank ds) 
  -> Dims ds 
  -> Idxs ds 
  -> (forall (ds' :: [Nat]). Dims ds' -> Idxs ds' -> r) 
  -> r
remapIdxs (Perm p) ds ix f = 
  T.reifyDims (P.permuteList p $ T.listDims ds) $ \ds' -> 
    f (T.reflect ds') (toIdxs (T.reflect ds') . fromIdxs ds $ ix)

majorToMinor :: forall ds i. Integral i => Dims ds -> Idxs ds -> i
majorToMinor dims = fromIntegral . go 1 dims
  where
    go :: forall ns . Word -> Dims ns -> Idxs ns -> Word
    go _ U U                     = 0
    go m (d :* ds) (Idx i :* is) = m * (i - 1) + go (m * T.dimVal d) ds is

minorToMajor :: forall ds i. Integral i => Dims ds -> Idxs ds -> i
minorToMajor d i = majorToMinor (_reversed d) (_reversed i)

fromIdxs :: forall ds i. Integral i => Dims ds -> Idxs ds -> i
fromIdxs = minorToMajor

toIdxs :: forall ds i. Integral i => Dims ds -> i -> Idxs ds
toIdxs dsd i = go dsd $ fromIntegral i
  where
    go :: forall ns . Dims ns -> Word -> Idxs ns
    go U 0 = U
    go U _ = error ("Idxs " ++ show (T.listDims dsd))
    go (T.Snoc ds d) off = case divMod off (T.dimVal d) of
      (off', j) -> go ds off' `T.snoc` Idx (j+1)

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
toIdxs :: forall ds i. Integral i => Dims ds -> i -> Idxs ds
toIdxs dds i = go dds $ fromIntegral i
  where
    go :: forall ns . Dims ns -> Word -> Idxs ns
    go U 0 = U
    go U _ = error ("Idxs " ++ show (listDims dds))
    go (d :* ds) off = case divMod off (dimVal d) of
      (off', j) -> Idx (j+1) :* go ds off'


--TODO why wont this compile
fromIdxs :: Dims ds -> Idxs ds -> Int
fromIdxs dsd = fromIntegral . go 1 dsd
  where
    go :: forall ns . Word -> Dims ns -> Idxs ns -> Word
    go _ U U                     = 0
    go m (T.Snoc ds d) (T.Snoc is (Idx i)) = m * (i - 1) + go (m * dimVal d) ds is
-}

-- TODO this is broken in the upstream lib
idxsFromWords :: forall ds . KnownDims ds => [Word] -> Maybe (Idxs ds)
idxsFromWords = unsafeCoerce . go (T.listDims (T.dims @_ @ds))
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
    dw = T.dimVal d
    k' is = go 1
      where
        go i
          | i > dw = return ()
          | otherwise = k (is `T.snoc` Idx i) >> go (i+1)



{-
 -
-- test -

--reify :: a -> (forall s. Reifies s a => Proxy s -> r) -> r

reify d233 $ \p -> totalDim' $ reflect p

reifyDims (reverse [2,3,3]) $ \p -> overDimIdx_ (reflect p) print
> reifyDims (reverse [2,3,3]) $ \p -> overDimIdx_ (reflect p) print
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

> overDimIdx_ (dims @_ @'[2,3,3]) print
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

> overDimIdx_ (dims @_ @'[2,3,3]) (\i -> remapIdxs re (dims @_ @'[2,3,3]) i print)
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

overDimIdx_ f print
overDimIdx_ f (\i -> remapIdxs re f i (\_ j -> print j))



-}

