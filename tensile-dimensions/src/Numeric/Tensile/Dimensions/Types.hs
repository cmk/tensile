{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-} 
module Numeric.Tensile.Dimensions.Types (
  module Numeric.Tensile.Dimensions.Types,
  module Numeric.Type.Evidence,
  module Numeric.Type.List,
  L.TypedList(..),
  Nat(..),
  KnownNat(..),
  natVal,
  SomeNat(..)
) where


import Data.Type.Bool
import Data.Type.Equality
import GHC.Exts              (Constraint)
import GHC.TypeLits
import Numeric.Type.Evidence
import Numeric.Type.List  -- (type(+:),(+:))

import qualified Numeric.Tensile.Dimensions.Types.List as L

impossible :: a
impossible = error "Numeric.Tensile: impossible"

type x < y = (CmpNat x y) ~ 'LT

type Permutable d d' = (L.Sort d ~ L.Sort d')

type Reshapable d d' = (Size d ~ Size d')

type family Rank (xs :: [k]) :: Nat where
    Rank '[] = 0
    Rank (_ ': xs) = 1 + Rank xs

type family Size (xs :: [Nat]) :: Nat where
    Size '[] = 1
    Size (x ': xs) = x * Size xs



class Reifies s a | s -> a where
  -- | Recover a value inside a 'reify' context, given a proxy for its reified type.
  reflect :: proxy s -> a

