{-# LANGUAGE TypeFamilies #-}
-- |
module Numeric.LALA.VecSpace where

class AdditiveMonoid v where
  zeroV :: v
  (.+.) :: v -> v -> v

class AdditiveMonoid v => AdditiveGroup v where
  negateV :: v -> v
  (.-.)   :: v -> v -> v

class AdditiveGroup v => VectorSpace v where
  type Scalar v :: *
  (*.) :: Scalar v -> v -> v
  (.*) :: v -> Scalar v -> v


