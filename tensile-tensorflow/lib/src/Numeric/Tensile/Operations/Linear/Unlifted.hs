module Numeric.Tensile.Operations.Linear.Unlifted where

import Numeric.Tensile.Dimensions
import Numeric.Tensile.Tensor

import qualified Numeric.Tensile.Operations.Linear.Internal as I

transpose
  :: forall d d' e. Elt e 
  => KnownDims d
  => Permutable d d'
  => Perm (Rank d) -> Tensor d e -> Tensor d' e
transpose = I.transpose (dims @d)

{-
infixl 7 <#>
(<#>)  
  :: forall a x y. KnownDim a
  => Dimensions x
  => Dimensions y
  => T (x +: a) -> T (a :+ y) -> T (x ++ y)
(<#>) = matmul


import Numeric.Type.List (Reverse(..))
import Unsafe.Coerce

rev :: forall (d :: [Nat]). Dims d -> Dims (Reverse d)
rev = unsafeCoerce . reverse . unsafeCoerce


transpose'
  :: forall d e. Elt e 
  => Dims d -> Tensor d e -> Tensor (Reverse d) e
transpose' d  = unsafeCoerce . transpose d (reversal' d) . unsafeCoerce


transpose' 
  :: forall d e. Elt e 
  => Dims d -> Tensor d e -> Tensor (Reverse d) e
transpose' d = transpose d p 
  where p = reversal' d
-}
