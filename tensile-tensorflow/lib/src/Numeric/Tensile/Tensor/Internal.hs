{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}

module Numeric.Tensile.Tensor.Internal where

import Control.Monad.ST (ST(..))
import Data.Bits
import Data.Int
import Data.Kind
import Data.Proxy
import Data.Word
import Data.Vector.Storable (Storable(..))
import Unsafe.Coerce (unsafeCoerce)

import Numeric.Tensile.Dimensions.Types
import Numeric.Tensile.Dimensions.Index

