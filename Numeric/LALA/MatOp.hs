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

-- | Generic API for matrices. We put constraint on elements which are
--   possible to store in matrix.
--
--   We also may need to put constraints on vector
class IMatrix m a where
  type Storage m :: (* -> *) -> * -> Constraint
  column :: (Storage m f a) => m f g a -> Int -> f a
  row    :: (Storage m g a) => m f g a -> Int -> g a

-- | Obtain size of a vector
class SizedVector v a where
  len :: v a -> Int

-- | Statically known size of a vector
class SizedVector v a => StaticVector v a where
  slen :: p (v a) -> Int

-- | Matrix-vector multiplication
class IMatrix m a => MulMV m a where
  (*!) :: (Storage m f a, Storage m g a) => m f g a -> g a -> f a

-- | Matrix-matrix multiplication
class (IMatrix m1 a, IMatrix m2 a) => MulMM m1 m2 a where
  type MM m1 m2 :: (* -> *) -> (* -> *) -> * -> *
  (!*!) :: m1 f g a -> m2 g h a -> (MM m1 m2) f h a
