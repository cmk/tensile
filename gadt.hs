data Shape = D1 Nat | D2 Nat Nat | D3 Nat Nat Nat
 
--deriving instance Generic (T ('D3 a b c))

--,import GHC.Generics, -XDeriveGeneric

import Data.Functor.Compose

data T (s :: Shape) t where
  T1 :: ( Dim a )
     => Compose (V a) Identity t
     -> T ('D1 a) t

  T2 :: ( Dim a, Dim b )
     => Compose (V a) (Compose (V b) Identity) t
     -> T ('D2 a b) t

  T3 :: ( Dim a
        , Dim b
        , Dim c
        )
     => Compose (V a) (Compose (V b) (Compose (V c) Identity)) t
     -> T ('D3 a b c) t deriving (Generic)

_T3 :: (Dim a, Dim b, Dim c) => Iso' (V a (V b (V c t))) (T ('D3 a b c) t)
_T3 = undefined


instance Functor (T ('D3 a b c)) where
  fmap f (T3 as) = T3 (fmap f as)

instance Foldable (T ('D3 a b c)) where
  foldMap f (T3 as) = foldMap f as

instance (Dim a, Dim b, Dim c) => Distributive (T ('D3 a b c)) where
  distribute as = T3 . fmap distribute . 

instance (Dim a, Dim b, Dim c) => Applicative (T ('D3 a b c)) where
  pure = T3 . pure 
  T3 as <*> T3 bs = T3 (as <*> bs)

instance (Dim a, Dim b, Dim c) => Traversable (T ('D3 a b c)) where
  

instance (Dim a, Dim b, Dim c) => Additive (T ('D3 a b c)) where
  zero = pure 0
  liftU2 f (T3 as) (T3 bs) = T3 (f <$> as <*> bs)
  liftI2 f (T3 as) (T3 bs) = T3 (f <$> as <*> bs)

instance (Dim a, Dim b, Dim c) => Finite (T ('D3 a b c)) where
  type Size (T ('D3 a b c)) = a * b * c
  toV = undefined
  fromV = undefined

type family TypeError (a :: ErrorMessage) :: b

-- TODO: try and only write this once for all T?
instance (Dim a, Dim b, Dim c, Num t) => Num (T ('D3 a b c) t) where
  T3 as + T3 bs = T3 $ (+) <$> as <*> bs
  {-# INLINE (+) #-}
  T3 as - T3 bs = T3 $ (-) <$> as <*> bs
  {-# INLINE (-) #-}
  T3 as * T3 bs = T3 $ (*) <$> as <*> bs
  {-# INLINE (*) #-}
  negate = fmap negate
  {-# INLINE negate #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}



