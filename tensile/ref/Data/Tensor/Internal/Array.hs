{-# LANGUAGE BangPatterns, FlexibleInstances, KindSignatures, MagicHash, PolyKinds, TypeOperators, UnboxedSums, UnboxedTuples, UndecidableInstances #-}

module Data.Tensor.Internal.Array where

import Data.Bits
import Data.Word (Word8)
import GHC.TypeLits
import Numeric.Dimensions
import Data.Vector.Primitive (Vector(..), Prim(..))
import qualified Data.Vector.Primitive as P


