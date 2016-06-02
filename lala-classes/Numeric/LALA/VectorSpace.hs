{-# LANGUAGE TypeFamilies #-}
-- |
-- Necessary type classes for operations with vector spaces
module Numeric.LALA.VectorSpace where

import Data.Word

----------------------------------------------------------------
-- Type classes
----------------------------------------------------------------

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
  -- | Get inverse element under addition
  negateV :: v -> v
  negateV x = zeroV - x
  -- | Subtraction
  (.-.)   :: v -> v -> v
  x .-. y = x .+. negateV y

-- | Vector space with given scalar type
class AdditiveGroup v => VectorSpace v where
  type Scalar v :: *
  (*.) :: Scalar v -> v -> v
  (.*) :: v -> Scalar v -> v


----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance AdditiveMonoid Int where
  zeroV = 0
  (.+.) = (+)
instance AdditiveMonoid Integer where
  zeroV = 0
  (.+.) = (+)
instance AdditiveMonoid Float where
  zeroV = 0
  (.+.) = (+)
instance AdditiveMonoid Double where
  zeroV = 0
  (.+.) = (+)


instance AdditiveGroup Int where
  negateV = negate
  (.-.) = (-)
instance AdditiveGroup Integer where
  negateV = negate
  (.-.) = (-)
instance AdditiveGroup Float where
  negateV = negate
  (.-.) = (-)
instance AdditiveGroup Double where
  negateV = negate
  (.-.) = (-)
