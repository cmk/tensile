{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE LambdaCase           #-}

{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE UndecidableInstances   #-} 
{-# LANGUAGE ViewPatterns           #-}
module Numeric.Tensile.Dimensions.Dim (
  module Numeric.Tensile.Dimensions.Dim,
  module Numeric.Tensile.Dimensions.Dim.Types
) where

import GHC.TypeLits
import Numeric.Tensile.Dimensions.Types (Reflects(..), type (<))
import Numeric.Tensile.Dimensions.Dim.Types
import Numeric.Type.Evidence

refineDim :: forall d. KnownDim d => (Dim d -> Bool) -> Maybe (Dim d)
refineDim p = reflectDim $ \x -> if p x then Just x else Nothing
