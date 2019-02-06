module Numeric.Tensile.Permutation where

import qualified Numeric.Tensile.Types as T
import qualified Math.Combinat.Permutations as P

newtype Perm (n :: T.Nat) = Perm { unPerm :: P.Permutation } deriving (Eq, Ord, Show)

instance Semigroup (Perm n) where
  (Perm p) <> (Perm q) = Perm $ P.multiply p q

instance T.KnownDim n => Monoid (Perm n) where
  mempty = Perm $ P.identity (fromIntegral $ T.dimVal' @n)

cycles (Perm t) = P.permutationToDisjointCycles $ t

reversal :: forall n. T.KnownDim n => Perm n
reversal = Perm $ P.reversePermutation n
  where n = fromIntegral $ T.dimVal' @n 

reversal' :: Word -> Perm n
reversal' n = Perm $ P.reversePermutation (fromIntegral n)

transposition' :: forall n. T.KnownDim n => Word -> Word -> Maybe (Perm n)
transposition' i j = if i <= n' && j <= n' then Just p else Nothing
  where
    p = Perm $ P.transposition n (fromIntegral i, fromIntegral j)
    n = fromIntegral $ T.dimVal' @n 
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
    n = fromIntegral $ T.dimVal' @n 
    i = fromIntegral $ T.dimVal' @i 
    j = fromIntegral $ T.dimVal' @j 

--ttt :: Perm 3
--ttt = transposition @2 @3

-}



