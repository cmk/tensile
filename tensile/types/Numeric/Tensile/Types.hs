{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE UndecidableInstances   #-}  
module Numeric.Tensile.Types (
  Data.Singletons.Prelude.List.Sort,
  module Numeric.Dim,
  module Numeric.Dimensions.Dims,
  module Numeric.Type.Evidence,
  module Numeric.TypedList, 
  module Numeric.Tensile.Types
) where

import Data.Proxy
import Data.Singletons.Prelude.List (Sort(..))
import Numeric.Dim
import Numeric.Dimensions.Dims
import Numeric.Type.Evidence
import Numeric.TypedList
import Unsafe.Coerce (unsafeCoerce)

impossible :: a
impossible = error "Numeric.Tensile: impossible"

type family Rank (xs :: [k]) :: Nat where
    Rank '[] = 0
    Rank (_ ': xs) = 1 + Rank xs

type family Size (xs :: [k]) :: Nat where
    Size '[] = 1
    Size (x ': xs) = x * Size xs

type KnownDims = Dimensions

type Permutable ds ds' = (Sort ds ~ Sort ds')
type Reshapable ds ds' = (Size ds ~ Size ds')

class Reifies s a | s -> a where
  -- | Recover a value inside a 'reify' context, given a proxy for its reified type.
  reflect :: proxy s -> a

instance KnownDim (d :: Nat) => Reifies d (Dim d) where
  reflect _ = dim

instance KnownDims (ds :: [Nat]) => Reifies ds (Dims ds) where
  reflect _ = dims

newtype MagicDim r = MagicDim (forall (d :: Nat). KnownDim d => Proxy d -> r)

newtype MagicDims r = MagicDims (forall (ds :: [Nat]). KnownDims ds => Proxy ds -> r)

reifyDims :: forall r. [Word] -> (forall (ds :: [Nat]). KnownDims ds => Proxy ds -> r) -> r
reifyDims ds k = unsafeCoerce (MagicDims k :: MagicDims r) ds Proxy

reifyDim :: forall r. Word -> (forall (d :: Nat). KnownDim d => Proxy d -> r) -> r
reifyDim d k = unsafeCoerce (MagicDim k :: MagicDim r) d Proxy




