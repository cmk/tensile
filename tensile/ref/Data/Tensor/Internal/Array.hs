{-# LANGUAGE BangPatterns, FlexibleInstances, KindSignatures, MagicHash, TypeOperators, UnboxedSums, UnboxedTuples, UndecidableInstances #-}

module Data.Tensor.Internal.Array where

import Data.Bits
import Data.Singletons.Prelude.List (Product)
import Data.Word (Word8)
import GHC.Base hiding (foldr)
import GHC.TypeLits
import Numeric.Dimensions --(Dimensions(..), KnownDim(..), dimVal)
--import Numeric.DataFrame
import Numeric.DataFrame.Internal.Array.Family.ArrayBase
--import Numeric.DataFrame.Internal.Array.Class
import Numeric.DataFrame.Internal.Array.PrimOps

import Numeric.PrimBytes

unSc :: ArrayBase (t :: Type) ('[] :: [Nat]) -> t
unSc = unsafeCoerce#

-- | Broadcast element into array
broadcast :: PrimBytes t => t -> ArrayBase t ds
broadcast t = ArrayBase (# t | #)
{-# INLINE broadcast #-}

-- | Index an array given an offset
ix# :: PrimBytes t => Int# -> ArrayBase t ds -> t
ix# i (ArrayBase a) = case a of
  (# t | #)                 -> t
  (# | (# off, _, arr #) #) -> indexArray arr (off +# i)
{-# INLINE ix# #-}

-- | Generate an array using an accumulator funtion
gen# 
  :: forall s t ds. PrimBytes t 
  => Int# -- ^ number of elements, not checked!
             --   Avoid using this argument if possible.
  -> (s -> (# s, t #))
  -> s -> (# s, ArrayBase t ds #)
gen# n f z0 = go (byteSize @t undefined *# n)
  where
    go bsize = case runRW#
     ( \s0 -> case newByteArray# bsize s0 of
         (# s1, mba #) -> case loop0 mba 0# z0 s1 of
           (# s2, z1 #) -> case unsafeFreezeByteArray# mba s2 of
             (# s3, ba #) -> (# s3, (# z1, ba #) #)
     ) of (# _, (# z1, ba #) #) -> (# z1, ArrayBase (# | (# 0# , n , ba #) #) #)
    {-# NOINLINE go #-}
    loop0 mba i z s
      | isTrue# (i ==# n) = (# s, z #)
      | otherwise = case f z of
          (# z', x #) -> loop0 mba (i +# 1#) z' (writeArray mba i x s)
{-# INLINE gen# #-}

-- | Update a single element in an array given an offset.
upd# :: PrimBytes t => Int# -> Int# -> t -> ArrayBase t ds -> ArrayBase t ds
upd# n i x (ArrayBase (# a | #)) = go (byteSize x)
  where
    go tbs = case runRW#
     ( \s0 -> case newByteArray# (tbs *# n) s0 of
         (# s1, mba #) -> unsafeFreezeByteArray# mba
           (writeArray mba i x
             (loop1# n (\j -> writeArray mba j a) s1)
           )
     ) of (# _, r #) -> ArrayBase (# | (# 0# , n , r #) #)
    {-# NOINLINE go #-}
upd# _ i x (ArrayBase (# | (# offN , n , ba #) #)) = go (byteSize x)
  where
    go tbs = case runRW#
     ( \s0 -> case newByteArray# (tbs *# n) s0 of
         (# s1, mba #) -> unsafeFreezeByteArray# mba
           (writeArray mba i x
             (copyByteArray# ba (offN *# tbs) mba 0# (tbs *# n) s1)
           )
     ) of (# _, r #) -> ArrayBase (# | (# 0# , n , r #) #)
    {-# NOINLINE go #-}
{-# INLINE upd# #-}

-- | Set a new value to an element
update
  :: forall t as bs asbs. PrimBytes t
  => ConcatList as bs asbs
  => Dimensions as
  => Dimensions bs
  => Dimensions asbs
  => Idxs bs -> ArrayBase t as -> ArrayBase t asbs -> ArrayBase t asbs
update ei x df
  | I# i <- fromEnum ei
  , I# len <- fromIntegral $ totalDim' @asbs
  = case runRW#
      ( \s0 -> case newByteArray# (len *# byteSize @t undefined) s0 of
        (# s1, mba #) -> unsafeFreezeByteArray# mba
          ( writeArray mba i x
            ( writeBytes mba 0# df s1 )
          )
      ) of (# _, r #) -> fromElems 0# len r

-- | Offset of an array in number of elements
elemOffset :: PrimBytes t => ArrayBase t ds -> Int#
elemOffset (ArrayBase a) = case a of
  (# _ | #)               -> 0#
  (# | (# off, _, _ #) #) -> off
{-# INLINE elemOffset #-}

-- | Number of elements in an array.
--   Returns zero if this information is not available at runtime.
--   This is possible only if all elements are same in an array.
elemSize0 :: PrimBytes t => ArrayBase t ds -> Int# 
elemSize0 (ArrayBase a) = case a of
  (# _ | #)             -> 0#
  (# | (# _, n, _ #) #) -> n
{-# INLINE elemSize0 #-}

-- | Get array by its offset and size in a ByteArray.
--   Both offset and size are given in element number.
fromElems :: PrimBytes t => Int# -> Int# -> ByteArray# -> ArrayBase t ds
fromElems off n ba = ArrayBase (# | (# off , n , ba #) #)
{-# INLINE fromElems #-}

-- | Unsafely get a sub-dataframe by its primitive element offset.
--   The offset is not checked to be aligned to the space structure or for bounds.
--   Arguments are zero-based primitive element offset and subset ("as" element) size (aka `totalDim` of sub dataframe)
--
--   Normal indexing can be expressed in terms of `indexOffset#`:
--
--   > i !. x = case (# dimVal (dim @as), fromEnum i #) of (# I# n, I# j #) -> indexOffset# (n *# j) n x
--
indexOffset# 
  :: forall t as bs asbs. PrimBytes t
  => Dimensions as
  => Dimensions bs
  => Dimensions asbs
  => ConcatList as bs asbs
  => Int# -- ^ Prim element offset
  -> Int# -- ^ Number of prim elements in the prefix subspace
  -> ArrayBase t asbs -> ArrayBase t as
indexOffset# i l d = case elemSize0 d of
  -- if elemSize0 returns 0, then this is a fromScalar-like constructor
  0# -> broadcast (ix# 0# d)
  _  -> fromElems (elemOffset d +# i) l (getBytes d)

-- | Get an element by its index in the dataframe
(!.) 
  :: forall t as bs asbs. PrimBytes t
  => Dimensions as
  => Dimensions bs
  => Dimensions asbs
  => ConcatList as bs asbs
  => Idxs bs -> ArrayBase t asbs -> ArrayBase t as
(!.) i = case (# totalDim (dims @_ @as), fromEnum i #) of
   (# W# n, I# j #) -> indexOffset# (word2Int# n *# j) (word2Int# n)
{-# INLINE [1] (!.) #-}
infixr 4 !.

-- | Zip two spaces on a specified subspace index-wise (with index)
iwzip 
  :: forall r s t as as' as'' bs asbs asbs' asbs''. (PrimBytes r, PrimBytes s, PrimBytes t)
  => Dimensions as
  => Dimensions as'
  => Dimensions as''
  => Dimensions bs
  => Dimensions asbs
  => Dimensions asbs'
  => Dimensions asbs''
  => ConcatList as bs asbs
  => ConcatList as' bs asbs'
  => ConcatList as'' bs asbs''
  => (Idxs bs -> ArrayBase t as -> ArrayBase s as' -> ArrayBase r as'')
  -> ArrayBase t asbs -> ArrayBase s asbs' -> ArrayBase r asbs''
iwzip f dft dfs = iwmap g dft
  where
    g i dft' = f i dft' (i !. dfs)
{-# INLINE iwzip #-}

-- | Map a function over each element with its index.
iwmap
  :: forall s t as as' bs asbs asbs'. (PrimBytes s, PrimBytes t)
  => Dimensions as
  => Dimensions as'
  => Dimensions bs
  => Dimensions asbs
  => Dimensions asbs'
  => ConcatList as bs asbs
  => ConcatList as' bs asbs'
  => (Idxs bs -> ArrayBase s as' -> ArrayBase t as)
  -> ArrayBase s asbs' -> ArrayBase t asbs
iwmap f df
  | elS <- byteSize @t undefined
  , dbs <- dims @_ @bs
  , W# lenBSW <- totalDim dbs
  , W# lenASW <- totalDim' @as
  , W# lenAS'W <- totalDim' @as'
  , lenBS <- word2Int# lenBSW
  , lenAS <- word2Int# lenASW
  , lenAS' <- word2Int# lenAS'W
  , lenASBS <- lenAS *# lenBS
  = case runRW#
      ( \s0 -> case newByteArray# (lenASBS *# elS) s0 of
        (# s1, mba #) -> unsafeFreezeByteArray# mba
          ( overDim_# dbs
            ( \i pos ->
                writeArray mba pos (f i (indexOffset# (pos *# lenAS') lenAS' df))
            ) 0# 1# s1
          )
      ) of (# _, r #) -> fromElems 0# lenASBS r

-- | Zip two spaces on a specified subspace element-wise (without index)
ewzip
  :: forall r s t as as' as'' bs asbs asbs' asbs''. (PrimBytes r, PrimBytes s, PrimBytes t)
  => Dimensions as
  => Dimensions as'
  => Dimensions as''
  => Dimensions bs
  => Dimensions asbs
  => Dimensions asbs'
  => Dimensions asbs''
  => ConcatList as bs asbs
  => ConcatList as' bs asbs'
  => ConcatList as'' bs asbs''
  => (ArrayBase t as -> ArrayBase s as' -> ArrayBase r as'')
  -> ArrayBase t asbs -> ArrayBase s asbs' -> ArrayBase r asbs''
ewzip = iwzip . const
{-# INLINE ewzip #-}

-- | Map a function over each element of an array.
ewmap
  :: forall s t as as' bs asbs asbs'. (PrimBytes s, PrimBytes t)
  => Dimensions as
  => Dimensions as'
  => Dimensions bs
  => Dimensions asbs
  => Dimensions asbs'
  => ConcatList as bs asbs
  => ConcatList as' bs asbs'
  => (ArrayBase s as' -> ArrayBase t as)
  -> ArrayBase s asbs' -> ArrayBase t asbs
ewmap f df
  | elS <- byteSize @t undefined
  , W# lenBSW <- totalDim' @bs
  , W# lenASW <- totalDim' @as
  , W# lenAS'W <- totalDim' @as'
  , lenBS <- word2Int# lenBSW
  , lenAS <- word2Int# lenASW
  , lenAS' <- word2Int# lenAS'W
  , lenASBS <- lenAS *# lenBS
  , lenAS'BS <- lenAS' *# lenBS
  = case runRW#
      ( \s0 -> case newByteArray# (lenASBS *# elS) s0 of
        (# s1, mba #) -> unsafeFreezeByteArray# mba
          ( loopWithI# 0# lenAS' lenAS'BS
            (\i off -> writeArray mba i (f (indexOffset# off lenAS' df)))
            s1
          )
      ) of (# _, r #) -> fromElems 0# lenASBS r
{-# INLINE ewmap #-}

-- | Generate a ArrayBase by repeating an element
ewgen
  :: forall t as bs asbs. PrimBytes t
  => Dimensions as
  => Dimensions bs
  => Dimensions asbs
  => ConcatList as bs asbs
  => ArrayBase t as -> ArrayBase t asbs
ewgen x = case elemSize0 x of
  0# -> broadcast (ix# 0# x)
  1# -> broadcast (ix# 0# x)
  lenAS
    | elS <- byteSize @t undefined
    , W# lenBSW <- totalDim' @bs
    , lenBS <- word2Int# lenBSW
    , lenASBS <- lenAS *# lenBS
    , bsize <- lenASBS *# elS
    -> case runRW#
        ( \s0 -> case newByteArray# bsize s0 of
          (# s1, mba #) -> unsafeFreezeByteArray# mba
            ( loop# 0# (lenAS *# elS) bsize
              (\off -> writeBytes mba off x)
              s1
            )
        ) of (# _, r #) -> fromElems 0# lenASBS r
{-# INLINE [1] ewgen #-}

-- | Generate a ArrayBase by iterating a function (index -> element)
iwgen
  :: forall t as bs asbs. PrimBytes t
  => Dimensions as
  => Dimensions bs
  => Dimensions asbs
  => ConcatList as bs asbs
  => (Idxs bs -> ArrayBase t as) -> ArrayBase t asbs
iwgen f
  | elS <- byteSize @t undefined
  , dbs <- dims @_ @bs
  , W# lenBSW <- totalDim dbs
  , W# lenASW <- totalDim' @as
  , lenBS <- word2Int# lenBSW
  , lenAS <- word2Int# lenASW
  , lenASBS <- lenAS *# lenBS
  = case runRW#
      ( \s0 -> case newByteArray# (lenASBS *# elS) s0 of
        (# s1, mba #) -> unsafeFreezeByteArray# mba
          ( overDim_# dbs
            ( \i pos -> writeArray mba pos (f i)
            ) 0# 1# s1
          )
      ) of (# _, r #) -> fromElems 0# lenASBS r

-- | Left-associative fold of a ArrayBase.
--   The fold is strict, so accumulater is evaluated to WHNF;
--   but you'd better make sure that the function is strict enough to not
--   produce memory leaks deeply inside the result data type.
ewfoldl
  :: forall s t as bs asbs. (PrimBytes s, PrimBytes t)
  => Dimensions as
  => Dimensions bs
  => Dimensions asbs
  => ConcatList as bs asbs
  => (s -> ArrayBase t as -> s) -> s -> ArrayBase t asbs -> s
ewfoldl f x0 df
  | ba <- getBytes df
  = foldDimOff (dims @_ @bs) (\(I# o) acc -> f acc (fromBytes o ba))
      (I# (byteOffset df))
      (I# (byteSize @t undefined) * fromIntegral (totalDim' @as)) x0

-- | Left-associative fold of a ArrayBase with an index
--   The fold is strict, so accumulater is evaluated to WHNF;
--   but you'd better make sure that the function is strict enough to not
--   produce memory leaks deeply inside the result data type.
iwfoldl
  :: forall s t as bs asbs. (PrimBytes s, PrimBytes t)
  => Dimensions as
  => Dimensions bs
  => Dimensions asbs
  => ConcatList as bs asbs
  => (Idxs bs -> s -> ArrayBase t as -> s) -> s -> ArrayBase t asbs -> s
iwfoldl f x0 df
  | ba <- getBytes df
  = foldDim (dims @_ @bs) (\i (I# o) acc -> f i acc (fromBytes o ba))
      (I# (byteOffset df))
      (I# (byteSize @t undefined) * fromIntegral (totalDim' @as)) x0

-- | Right-associative fold of a ArrayBase
--   The fold is strict, so accumulater is evaluated to WHNF;
--   but you'd better make sure that the function is strict enough to not
--   produce memory leaks deeply inside the result data type.
ewfoldr
  :: forall s t as bs asbs. (PrimBytes s, PrimBytes t)
  => Dimensions as
  => Dimensions bs
  => Dimensions asbs
  => ConcatList as bs asbs
  => (ArrayBase t as -> s -> s) -> s -> ArrayBase t asbs -> s
ewfoldr f x0 df
  | step <- I# (byteSize @t undefined) * fromIntegral (totalDim' @as)
  , ba <- getBytes df
  = foldDimOff (dims @_ @bs) (\(I# o) -> f (fromBytes o ba))
      (I# (byteOffset df +# byteSize df) - step)
      (negate step) x0

-- | Right-associative fold of a ArrayBase with an index
--   The fold is strict, so accumulater is evaluated to WHNF;
--   but you'd better make sure that the function is strict enough to not
--   produce memory leaks deeply inside the result data type.
iwfoldr
  :: forall s t as bs asbs. (PrimBytes s, PrimBytes t)
  => Dimensions as
  => Dimensions bs
  => Dimensions asbs
  => ConcatList as bs asbs
  => (Idxs bs -> ArrayBase t as -> s -> s) -> s -> ArrayBase t asbs -> s
iwfoldr f x0 df
  | step <- I# (byteSize @t undefined) * fromIntegral (totalDim' @as)
  , ba <- getBytes df
  = foldDimReverse (dims @_ @bs) (\i (I# o) -> f i (fromBytes o ba))
      (I# (byteOffset df +# byteSize df) - step)
      step x0

-- | Apply an applicative functor on each element with its index
--     (Lens-like indexed traversal)
indexWise_
  :: forall f s t as bs asbs. (PrimBytes s, PrimBytes t, PrimBytes (f ()))
  => Applicative f
  => Dimensions as
  => Dimensions bs
  => Dimensions asbs
  => ConcatList as bs asbs
  => (Idxs bs -> ArrayBase t as -> f s)
  -> ArrayBase t asbs -> f ()
indexWise_ f = iwfoldr (\i -> (*>) . f i) (pure ())

-- | Apply an applicative functor on each element (Lens-like traversal)
elementWise_
  :: forall f s t as bs asbs. (PrimBytes s, PrimBytes t, PrimBytes (f ()))
  => Applicative f
  => Dimensions as
  => Dimensions bs
  => Dimensions asbs
  => ConcatList as bs asbs
  => (ArrayBase t as -> f s)
  -> ArrayBase t asbs -> f ()
elementWise_ f = ewfoldr ((*>) . f) (pure ())


{-

-- | Apply a functor over a single element (simple lens)
element
  :: forall f s t as bs asbs. (PrimBytes s, PrimBytes t)
  => Applicative f
  => Dimensions as
  => Dimensions bs
  => Dimensions asbs
  => ConcatList as bs asbs
  => Idxs bs
  -> (ArrayBase t as -> f (ArrayBase t as))
  -> ArrayBase t asbs -> f (ArrayBase t asbs)
element i f df = flip (update i) df <$> f (i !. df)
{-# INLINE element #-}

-- | Index an element (reverse of !.)
(!) = flip (!.)
infixl 4 !
{-# INLINE (!) #-}

ewfoldMap :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) m
           . (Monoid m, SubSpace t as bs asbs)
          => (ArrayBase t as -> m) -> ArrayBase t asbs -> m
ewfoldMap f = ewfoldl (\m b -> m `seq` (mappend m $! f b)) mempty
{-# INLINE ewfoldMap #-}

iwfoldMap :: forall t (as :: [Nat]) (bs :: [Nat]) (asbs :: [Nat]) m
           . ( Monoid m, SubSpace t as bs asbs)
          => (Idxs bs -> ArrayBase t as -> m) -> ArrayBase t asbs -> m
iwfoldMap f = iwfoldl (\i m b -> m `seq` (mappend m $! f i b)) mempty
{-# INLINE iwfoldMap #-}

-- | Apply an applicative functor on each element with its index
--     (Lens-like indexed traversal)
indexWise
  :: forall f s t as as' bs asbs asbs'. (PrimBytes s, PrimBytes t)
  => Applicative f
  => Dimensions as
  => Dimensions as'
  => Dimensions bs
  => Dimensions asbs
  => Dimensions asbs'
  => ConcatList as bs asbs
  => ConcatList as' bs asbs'
  => (Idxs bs -> ArrayBase s as' -> f (ArrayBase t as))
  -> ArrayBase s asbs' -> f (ArrayBase t asbs)
indexWise f df = runWithState <$> iwfoldl applyF (pure initialState) df
  where
    -- run a state-based continuation within RW
    runWithState :: ( State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
                 -> ArrayBase t asbs
    runWithState g = case runRW#
                       ( \s0 -> case g s0 of
                            (# s1, (# marr, _ #) #) -> unsafeFreezeByteArray# marr s1
                       ) of (# _, arr #) -> fromElems 0# rezLength# arr

    -- Prepare empty byte array for the result ArrayBase and keep a current position counter
    -- Input: state
    -- Output: state +
    --     ( current mutable byte array + current write position )
    initialState :: State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #)
    initialState s0 = case newByteArray# (rezLength# *# rezElBSize#) s0 of
                        (# s1, marr #) -> (# s1, (# marr, 0# #) #)

    -- Given the result chunk, write it into a mutable array
    updateChunk :: (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
                -> ArrayBase t as
                -> (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
    updateChunk g dfChunk = case (# byteOffset dfChunk, getBytes dfChunk #) of
        (# off#, arr#  #) -> \s -> case g s of
                                    (# s1, (# marr#, pos# #) #) -> case
                                        copyByteArray# arr# (off# *# rezElBSize#)
                                                       marr# (pos# *# rezElBSize#)
                                                       (rezStepN# *# rezElBSize#) s1 of
                                      s2 -> (# s2, (# marr#, pos# +# rezStepN# #) #)

    -- Apply applicative functor on each chunk and update a state.
    applyF :: Idxs bs
           -> f (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
           -> ArrayBase s as'
           -> f (State# RealWorld -> (# State# RealWorld, (# MutableByteArray# RealWorld, Int# #) #))
    applyF idx s dfChunk = idx `seq` dfChunk `seq` updateChunk <$> s <*> f idx dfChunk

    -- Element byte size of the result ArrayBase (byte size of s)
    rezElBSize# = byteSize @t undefined
    -- Number of primitive elements in the result ArrayBase chunk
    !(I# rezStepN#) = fromIntegral $ totalDim' @as
    -- Number of primitive elements in the result ArrayBase
    !(I# rezLength#) = fromIntegral $ totalDim' @asbs


-- | Apply an applicative functor on each element (Lens-like traversal)
elementWise :: forall s (as' :: [Nat]) (asbs' :: [Nat]) f
             . ( Applicative f
               , SubSpace s as' bs asbs'
               )
            => (ArrayBase s as' -> f (ArrayBase t as))
            -> ArrayBase s asbs' -> f (ArrayBase t asbs)
elementWise = indexWise . const
{-# INLINE elementWise #-}




-}

