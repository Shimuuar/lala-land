{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
module Data.Vector.Strided where

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M

import  Data.Vector.Strided.Mutable


data Strided v a = Strided
                   !Int         -- Stride
                   !(v a)       -- Underlying vector

type instance G.Mutable (Strided v) = MVector (G.Mutable v)

instance G.Vector v a => G.Vector (Strided v) a where
  basicUnsafeFreeze (MVector s mv) = do
    v <- G.basicUnsafeFreeze mv
    return $ Strided s v
  basicUnsafeThaw (Strided s v) = do
    mv <- G.basicUnsafeThaw v
    return $ MVector s mv
  basicLength (Strided s v) = G.basicLength v `div` s
  basicUnsafeSlice i j (Strided s v)
    = Strided s $ G.basicUnsafeSlice (i*s) (j*s) v
  basicUnsafeIndexM (Strided s v) i
    = G.basicUnsafeIndexM v (i*s)
