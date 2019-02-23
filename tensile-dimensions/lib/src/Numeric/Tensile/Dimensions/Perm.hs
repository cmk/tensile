{-# LANGUAGE AllowAmbiguousTypes #-}
module Numeric.Tensile.Dimensions.Perm where

import Numeric.Tensile.Dimensions.Dims
import Numeric.Tensile.Dimensions.Types
import Unsafe.Coerce (unsafeCoerce)

import qualified Math.Combinat.Permutations as P

type Permutable d d' = (Sort d ~ Sort d')

newtype Perm (n :: Nat) = Perm { unPerm :: P.Permutation } deriving (Eq, Ord, Show)

--newtype Perm' d d' = Perm { unPerm :: P.Permutation } deriving (Eq, Ord, Show)

instance Semigroup (Perm n) where
  (Perm p) <> (Perm q) = Perm $ P.multiply p q

-- TODO replace w/ KnownNat constraint
instance KnownDim n => Monoid (Perm n) where
  mempty = Perm $ P.identity (fromIntegral $ reflectDim @n fromDim)

-- TODO remark as unsafe or remove.
--unsafePermute :: Permutable d d' => Perm (Rank d) -> TypedList f d -> TypedList f d'
unsafePermute :: Perm (Rank d) -> TypedList f d -> TypedList f d'
unsafePermute (Perm p) = unsafeCoerce . P.permuteList p . unsafeCoerce

identity :: forall ds. Dims ds -> Perm (Rank ds)
identity = Perm . P.identity . fromIntegral . length . fromDims 

reversal :: forall ds. Dims ds -> Perm (Rank ds)
reversal = Perm . P.reversePermutation . length . fromDims

transposition :: forall n. KnownDim n => Word -> Word -> Maybe (Perm n)
transposition i j = if i <= n' && j <= n' then Just p else Nothing
  where
    p = Perm $ P.transposition n (fromIntegral i, fromIntegral j)
    n = fromIntegral $ reflectDim @n fromDim
    n' = fromIntegral n


{-
transposition'' :: Word -> Word -> Word -> Maybe (Perm n)
transposition'' n i j = 
  reifySomeDim n $ \n ->
    reifySomeDim i $ \i -> 
      reifySomeDim j $ \j -> transposition' (reflect i) (reflect j)

-- TODO clean up type sig
transposition
  :: forall i j n. (i <= n, j <= n)
  => (KnownDim i, KnownDim j, KnownDim n) --All KnownDim [i,j,n]
  => Perm n
transposition = Perm $ P.transposition n (i,j)
  where
    n = fromIntegral $ fromDim @n 
    i = fromIntegral $ fromDim @i 
    j = fromIntegral $ fromDim @j 

--ttt :: Perm 3
--ttt = transposition @2 @3

-}



