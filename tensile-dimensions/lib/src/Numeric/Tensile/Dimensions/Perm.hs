{-# LANGUAGE AllowAmbiguousTypes #-}
module Numeric.Tensile.Dimensions.Perm where

import Numeric.Tensile.Dimensions.Dim
import Numeric.Tensile.Dimensions.Dims
import Numeric.Tensile.Dimensions.Types
import Unsafe.Coerce (unsafeCoerce)

import qualified Math.Combinat.Permutations as P

type Permutable d d' = (Sort d ~ Sort d')

newtype Perm (n :: Nat) = Perm { unPerm :: P.Permutation } deriving (Eq, Ord, Show)

--newtype Perm' d d' = Perm { unPerm :: P.Permutation } deriving (Eq, Ord, Show)

instance Semigroup (Perm n) where
  (Perm p) <> (Perm q) = Perm $ P.multiply p q

instance KnownDim n => Monoid (Perm n) where
  mempty = Perm $ P.identity (fromIntegral $ fromDim @n)

cycles (Perm t) = P.permutationToDisjointCycles $ t

-- TODO remark as unsafe or remove.
--unsafePermute :: Permutable d d' => Perm (Rank d) -> TypedList f d -> TypedList f d'
unsafePermute :: Perm (Rank d) -> TypedList f d -> TypedList f d'
unsafePermute (Perm p) = unsafeCoerce . P.permuteList p . unsafeCoerce

reversal :: forall d. KnownDims d => Perm (Rank d)
reversal = reversal' (dims @d)

reversal' :: forall d. Dims d -> Perm (Rank d)
reversal' = Perm . P.reversePermutation . length . fromDims'

transposition' :: forall n. KnownDim n => Word -> Word -> Maybe (Perm n)
transposition' i j = if i <= n' && j <= n' then Just p else Nothing
  where
    p = Perm $ P.transposition n (fromIntegral i, fromIntegral j)
    n = fromIntegral $ fromDim @n 
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
    n = fromIntegral $ fromDim' @n 
    i = fromIntegral $ fromDim' @i 
    j = fromIntegral $ fromDim' @j 

--ttt :: Perm 3
--ttt = transposition @2 @3

-}



