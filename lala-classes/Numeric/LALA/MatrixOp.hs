-- |
-- Operations with matrices
module Numeric.LALA.MatrixOp where


-- | Transposition of matrix
newtype Trans m a = Trans { unTranspose :: m a }

-- | Hermitian conjugate of matrix
newtype Herm m a = Herm { unHerm :: m a }


-- | Type class for matrix-vector multiplication
class MulMV m v a where
  -- | Multiplicate matrix by column vector 
  (!*) :: m a -> v a -> v a
  -- | Multiplicate matrix by row vector
  (!*) :: v a -> m a -> v a

class MulMM m1 m2 a where
  type M m1 m2 :: * -> *
  (!*!) :: m1 a -> m2 a -> M m1 m2 a
