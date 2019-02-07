{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}  
module Numeric.Tensile.Types (
  Data.Singletons.Prelude.List.Sort,
  Numeric.TypedList.TypedList(..),
  Numeric.TypedList.snoc,
  Dims(..),
  Dimensions(..),
  KnownDims(..),
  listDims,
  module Numeric.Dim,
  module Numeric.Type.Evidence,
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

type family Size (xs :: [Nat]) :: Nat where
    Size '[] = 1
    Size (x ': xs) = x * Size xs

type KnownDims = Dimensions

type Permutable d d' = (Sort d ~ Sort d')
type Reshapable d d' = (Size d ~ Size d')

class Reifies s a | s -> a where
  -- | Recover a value inside a 'reify' context, given a proxy for its reified type.
  reflect :: proxy s -> a

instance KnownDim (d :: Nat) => Reifies d (Dim d) where
  reflect _ = dim

instance KnownDims (d :: [Nat]) => Reifies d (Dims d) where
  reflect _ = dims

newtype MagicDim' r = MagicDim' (forall (d :: Nat). KnownDim d => Proxy d -> r)



reifyDim' :: forall r. Word -> (forall (d :: Nat). KnownDim d => Proxy d -> r) -> r
reifyDim' d k = unsafeCoerce (MagicDim' k :: MagicDim' r) d Proxy


newtype MagicDims d r = MagicDims (KnownDims d => r)
newtype MagicDims' r = MagicDims' (forall (d :: [Nat]). KnownDims d => Proxy d -> r)

reifyDims :: forall d r . Dims d -> (KnownDims d => r) -> r
reifyDims d k = unsafeCoerce (MagicDims k :: MagicDims d r) d

reifyDims' :: forall r. [Word] -> (forall (d :: [Nat]). KnownDims d => Proxy d -> r) -> r
reifyDims' d k = unsafeCoerce (MagicDims' k :: MagicDims' r) d Proxy

withDims :: Dims d -> Evidence (KnownDims d)
withDims d = reifyDims d E

{-
compareDims' :: Dims as -> Dims bs -> Ordering
compareDims' a b = compare (listDims a) (listDims b)
{-# INLINE compareDims #-}

compareDims :: forall as bs p q
              . (Dimensions as, Dimensions bs)
             => p as -> q bs -> Ordering
compareDims _ _ = compareDims' (dims @_ @as) (dims @_ @bs)

fromDims = listDims
compareDims
sameDims
totalDim


-}
