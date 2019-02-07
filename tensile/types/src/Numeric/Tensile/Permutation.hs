{-# LANGUAGE AllowAmbiguousTypes #-}
module Numeric.Tensile.Permutation where

import Numeric.Tensile.Types
import qualified Math.Combinat.Permutations as P

newtype Perm (n :: Nat) = Perm { unPerm :: P.Permutation } deriving (Eq, Ord, Show)

instance Semigroup (Perm n) where
  (Perm p) <> (Perm q) = Perm $ P.multiply p q

instance KnownDim n => Monoid (Perm n) where
  mempty = Perm $ P.identity (fromIntegral $ dimVal' @n)

cycles (Perm t) = P.permutationToDisjointCycles $ t

reversal :: forall d. Dims d -> Perm (Rank d)
reversal = Perm . P.reversePermutation . length . listDims

reversal' :: forall d. KnownDims d => Perm (Rank d)
reversal' = reversal (dims @_ @d)

transposition' :: forall n. KnownDim n => Word -> Word -> Maybe (Perm n)
transposition' i j = if i <= n' && j <= n' then Just p else Nothing
  where
    p = Perm $ P.transposition n (fromIntegral i, fromIntegral j)
    n = fromIntegral $ dimVal' @n 
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
    n = fromIntegral $ dimVal' @n 
    i = fromIntegral $ dimVal' @i 
    j = fromIntegral $ dimVal' @j 

--ttt :: Perm 3
--ttt = transposition @2 @3

-}



