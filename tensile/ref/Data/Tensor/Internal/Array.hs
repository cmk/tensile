{-# LANGUAGE BangPatterns, FlexibleInstances, KindSignatures, MagicHash, PolyKinds, TypeOperators, UnboxedSums, UnboxedTuples, UndecidableInstances #-}

module Data.Tensor.Internal.Array where

import Data.Bits
import Data.Word (Word8)
import Numeric.Tensile.Types
import Data.Vector.Primitive (Vector(..), Prim(..))
import qualified Data.Vector.Primitive as P


