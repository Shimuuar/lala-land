{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Matrix-vector and matrix-matrix multiplication
module Numeric.LALA.MatOp where

import GHC.Prim (Constraint)

----------------------------------------------------------------
-- Type classes
----------------------------------------------------------------

class SizedVector v a where
  len :: v a -> Int

class SizedVector v a => StaticVector v a where
  slen :: p (v a) -> Int


class MulMV m a where
  type Vec m :: (* -> *) -> * -> Constraint
  (*!) :: (Vec m f a, Vec m g a) => m f g a -> g a -> f a

