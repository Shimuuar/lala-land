{-# LANGUAGE TypeFamilies #-}
-- |
module Numeric.LALA.VecSpace where

-- | Laws for monoid
--
--   1. Usual monoid laws
--
--   2. If v is also Num
-- > zeroV = 0
-- > (.+.) = (+)
class AdditiveMonoid v where
  zeroV :: v
  (.+.) :: v -> v -> v

-- | Laws for group
--
--  1. Usual group laws
--
--  2. If v is also Num
-- > negateV = negate
-- > (.-.)   = (-)
class AdditiveMonoid v => AdditiveGroup v where
  negateV :: v -> v
  (.-.)   :: v -> v -> v

class AdditiveGroup v => VectorSpace v where
  type Scalar v :: *
  (*.) :: Scalar v -> v -> v
  (.*) :: v -> Scalar v -> v
