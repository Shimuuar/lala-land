-- |
module Data.Matrix.BLAS.Dense where

{-
http://www.netlib.org/lapack/lug/node121.html
-}

-- | General matrix
data Matrix f g a = Matrix

data MatKind = Gen
             | Symm
             | Herm

data PackedSymm f g a = PSymm

data PackedHerm f g a = PHerm

