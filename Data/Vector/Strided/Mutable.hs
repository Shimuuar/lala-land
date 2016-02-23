{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-- |
module Data.Vector.Strided.Mutable where

import qualified Data.Vector.Generic.Mutable as M


-- | Strided view on vector
data MVector v s a = MVector
                     !Int       -- Stride
                     !(v s a)   -- Underlying vector

instance M.MVector v a => M.MVector (MVector v) a where
  basicLength (MVector s v) = M.basicLength v `div` s
  basicUnsafeSlice i j (MVector s v)
    = MVector s (M.basicUnsafeSlice (i*s) (j*s) v)

  basicOverlaps (MVector _ v1) (MVector _ v2)
    = M.basicOverlaps v1 v2
  basicUnsafeNew n = do
    v <- M.basicUnsafeNew n
    return $! MVector 1 v
  basicInitialize  (MVector _ v)     = M.basicInitialize v
  basicUnsafeRead  (MVector s v) i   = M.basicUnsafeRead v (i*s)
  basicUnsafeWrite (MVector s v) i x = M.basicUnsafeWrite v (i*s) x
  {-# INLINE basicLength      #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps    #-}
  {-# INLINE basicUnsafeNew   #-}
  {-# INLINE basicInitialize  #-}
  {-# INLINE basicUnsafeRead  #-}
  {-# INLINE basicUnsafeWrite #-}
