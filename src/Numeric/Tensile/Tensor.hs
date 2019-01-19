{-# LANGUAGE DataKinds, GADTs     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
module Numeric.Tensile.Tensor where

import Data.Kind (Type)
import Data.Int (Int64)
import GHC.TypeLits (Nat)

import Numeric.Dimensions (Dimensions(..))
import qualified Numeric.Dimensions as D


-- see https://ghc.haskell.org/trac/ghc/ticket/4009
newtype T :: (Type -> Type) -> [Nat] -> Type -> Type where T :: f e -> T f s e deriving Show 

-- | return the a runtime shape representing the dimensions of a tensor.
shape :: forall f s e. Dimensions s => T f s e -> [Int64]
shape _ = fromIntegral <$> D.listDims (D.dims @Nat @s)

-- | Product of all dimension sizes @O(Length xs)@.
size :: forall f s e. Dimensions s => T f s e -> Int64
size = product . shape





--

