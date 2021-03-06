module Matrix (
    tests_Matrix
    ) where

import Control.Monad( replicateM, zipWithM_ )
import Data.AEq
import Data.List( transpose )
import Debug.Trace
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck hiding ( vector )
import qualified Test.QuickCheck as QC

import Numeric.LinearAlgebra
import qualified Numeric.LinearAlgebra.Matrix as M
import qualified Numeric.LinearAlgebra.Vector as V

import Test.QuickCheck.LinearAlgebra( TestElem(..), Dim2(..), Index2(..),
    Assocs2(..), MatrixPair(..) )
import qualified Test.QuickCheck.LinearAlgebra as Test

import Typed


tests_Matrix = testGroup "Matrix"
    [ testPropertyI "dim . fromList" prop_dim_fromList
    , testPropertyI "at . fromList" prop_at_fromList
    , testPropertyI "fromCols" prop_fromCols    
    , testPropertyI "fromRows" prop_fromRows        
    , testPropertyI "constant" prop_constant
    , testPropertyI "indices" prop_indices
    , testPropertyI "elems" prop_elems
    , testPropertyI "assocs" prop_assocs
    , testPropertyI "update" prop_update
    , testPropertyI "accum" prop_accum
    , testPropertyI "map" prop_map
    , testPropertyI "zipWith" prop_zipWith
    , testPropertyI "col" prop_col
    , testPropertyI "cols" prop_cols
    , testPropertyD "row" prop_row
    , testPropertyD "rows" prop_rows
    , testPropertyI "diag" prop_diag
    , testPropertyI "slice" prop_slice
    , testPropertyI "splitRowsAt" prop_splitRowsAt
    , testPropertyI "splitColsAt" prop_splitColsAt
    , testPropertyI "viewVector" prop_viewVector
    , testPropertyDZ "shiftDiag" prop_shiftDiag prop_shiftDiag
    , testPropertyDZ "shiftDiagWithScale"
        prop_shiftDiagWithScale prop_shiftDiagWithScale
    , testPropertyDZ "add" prop_add prop_add
    , testPropertyDZ "sub" prop_sub prop_sub
    , testPropertyDZ "scale" prop_scale prop_scale
    , testPropertyDZ "scaleRows" prop_scaleRows prop_scaleRows
    , testPropertyDZ "scaleCols" prop_scaleCols prop_scaleCols
    , testPropertyDZ "negate" prop_negate prop_negate
    , testPropertyDZ "conjugate" prop_conjugate prop_conjugate
    , testPropertyDZ "trans" prop_trans prop_trans
    , testPropertyDZ "conjTrans" prop_conjTrans prop_conjTrans
    , testPropertyDZ "rank1Update" prop_rank1Update prop_rank1Update
    , testPropertyDZ "mulVector" prop_mulVector prop_mulVector
    , testPropertyDZ "mulVectorWithScale" prop_mulVectorWithScale prop_mulVectorWithScale
    , testPropertyDZ "addMulVectorWithScales" prop_addMulVectorWithScales prop_addMulVectorWithScales    
    , testPropertyDZ "mulMatrix" prop_mulMatrix prop_mulMatrix
    , testPropertyDZ "mulMatrixWithScale" prop_mulMatrixWithScale prop_mulMatrixWithScale
    , testPropertyDZ "addMulMatrixWithScales" prop_addMulMatrixWithScales prop_addMulMatrixWithScales    

    ]



------------------------- Matrix Construction ------------------------------

prop_dim_fromList t (Dim2 (m,n)) =
    forAll (QC.vector $ m*n) $ \es -> let
        a = typed t $ M.fromList (m,n) es
        in M.dim a == (m,n)
        
prop_at_fromList t (Dim2 (m,n)) =
    forAll (QC.vector $ m*n) $ \es -> let
        a = typed t $ M.fromList (m,n) es
        in and [ M.at a (i,j) === e
               | ((i,j),e) <- zip [ (i,j) | j <- [ 0..n-1 ], i <- [ 0..m-1 ] ]
                                  es
               ]

prop_fromCols t (Dim2 (m,n)) =
    forAll (replicateM n $ Test.vector m) $ \cs ->
        M.fromCols (m,n) cs === (typed t $ M.fromList (m,n) $
            concatMap V.elems cs)

prop_fromRows t (Dim2 (m,n)) =
    forAll (replicateM m $ Test.vector n) $ \rs ->
        M.fromRows (m,n) rs === (typed t $ M.fromList (m,n) $
            concat $ transpose $ map V.elems rs)

prop_constant t (Dim2 (m,n)) e =
    M.constant (m,n) e === M.fromList (m,n) (replicate (m*n) e)
  where
    _ = typed t [e]


-------------------------- Accessing Matrices ------------------------------

prop_indices t x =
    M.indices x === [ (i,j) | j <- [ 0..n-1 ], i <- [ 0..m-1 ] ]
  where
    (m,n) = M.dim x
    _ = immutableMatrix x
    _ = typed t x

prop_elems t x =
    M.elems x === [ M.at x i | i <- M.indices x ]
  where
    _ = typed t x
    
prop_assocs t x =
    M.assocs x === zip (M.indices x) (M.elems x)
  where
    _ = typed t x


------------------------- Incremental Updates ------------------------------
    
prop_update t (Assocs2 mn ies) =
    forAll (typed t `fmap` Test.matrix mn) $ \x -> let
        x' = M.update x ies
        is = M.indices x
        is1 = (fst . unzip) ies
        is0 = [ i | i <- is, i `notElem` is1 ]
        in and $
            [ M.at x' i `elem` [ e | (i',e) <- ies, i' == i ]
            | i <- is1
            ] ++
            [ M.at x' i === M.at x i
            | i <- is0
            ]

prop_accum t (Blind f) (Assocs2 mn ies) =
    forAll (typed t `fmap` Test.matrix mn) $ \x -> let
        x' = M.accum f x ies
        in x' === M.fromList mn [ foldl f e [ e' | (i',e') <- ies, i' == i]
                                | (i,e) <- M.assocs x ]
  where
      _ = typed t $ (snd . unzip) ies

-------------------------- Derived Matrices ------------------------------
     
prop_map t (Blind f) x =
    M.map f x === M.fromList (M.dim x) (map f $ M.elems x)
  where
    _ = typed t x
    _ = typed t $ M.map f x

prop_zipWith t (Blind f) (MatrixPair x y) =
    M.zipWith f x y === (M.fromList (M.dim x) $
                                zipWith f (M.elems x) (M.elems y))
  where
    _ = typed t x
    _ = typed t y    
    _ = typed t $ M.zipWith f x y
     

------------------------------ Matrix Views --------------------------------

prop_col t (Index2 (m,n) (_,j)) =
    forAll (typed t `fmap` Test.matrix (m,n)) $ \a ->
        M.col a j === V.fromList m [ M.at a (i,j) | i <- [ 0..m-1 ] ]

prop_cols t a =
    M.cols a === [ M.col a j | j <- [ 0..n-1 ] ]
  where
    (_,n) = M.dim a
    _ = typed t $ immutableMatrix a

prop_row t (Index2 (m,n) (i,_)) =
    forAll (typed t `fmap` Test.matrix (m,n)) $ \a ->
        M.row a i === V.fromList n [ M.at a (i,j) | j <- [ 0..n-1 ] ]

prop_rows t a =
    M.rows a === [ M.row a i | i <- [ 0..m-1 ] ]
  where
    (m,_) = M.dim a
    _ = typed t $ immutableMatrix a

prop_diag t a =
    M.diag a === V.fromList mn [ M.at a (i,i) | i <- [ 0..mn-1 ] ]
  where
    (m,n) = M.dim a
    mn = min m n
    _ = typed t $ immutableMatrix a

prop_slice t a =
    forAll (choose (0,m)) $ \m' ->
    forAll (choose (0,n)) $ \n' ->
    forAll (choose (0,m-m')) $ \i ->
    forAll (choose (0,n-n')) $ \j ->
        M.slice (i,j) (m',n') a
            === M.fromCols (m',n') [ V.slice i m' (M.col a j')
                                      | j' <- [ j..j+n'-1 ] ]
  where
    (m,n) = M.dim a
    _ = typed t a

prop_splitRowsAt t a =
    forAll (choose (0,m)) $ \i ->
        M.splitRowsAt i a
            === ( M.slice (0,0) (i,n) a
                , M.slice (i,0) (m-i,n) a
                )
  where
    (m,n) = M.dim a
    _  = typed t $ immutableMatrix a

prop_splitColsAt t a =
    forAll (choose (0,n)) $ \j ->
        M.splitColsAt j a
            === ( M.slice (0,0) (m,j) a
                , M.slice (0,j) (m,n-j) a
                )
  where
    (m,n) = M.dim a
    _  = typed t $ immutableMatrix a

prop_viewVector t (Dim2 (m,n)) =
    forAll (Test.vector $ m*n) $ \x -> let _ = typed t x in
        M.elems (M.fromVector (m,n) x) === V.elems x


-------------------------- Num Matrix Operations --------------------------

prop_shiftDiag t a =
    forAll (Test.vector (min m n)) $ \d ->
        M.shiftDiag d a
            === M.accum (+) a [ ((i,i),e) | (i,e) <- V.assocs d ]
  where
    (m,n) = M.dim a
    _ = typed t a

prop_shiftDiagWithScale t k a =
    forAll (Test.vector (min m n)) $ \d ->
        M.shiftDiagWithScale k d a
            ~== M.accum (+) a [ ((i,i),k * e) | (i,e) <- V.assocs d ]
  where
    (m,n) = M.dim a
    _ = typed t a

prop_add t (MatrixPair x y) =
    x `M.add` y === M.zipWith (+) x y
  where
    _ = typed t x

prop_sub t (MatrixPair x y) =
    x `M.sub` y === M.zipWith (-) x y
  where
    _ = typed t x

prop_scale t k x =
    M.scale k x ~== M.map (k*) x
  where
    _ = typed t x

prop_scaleRows t a =
    forAll (Test.vector m) $ \s ->
        M.scaleRows s a
            ~== M.fromCols (m,n) [ V.mul s x | x <- M.cols a ]
  where
    (m,n) = M.dim a
    _ = typed t a

prop_scaleCols t a =
    forAll (Test.vector n) $ \s ->
        M.scaleCols s a
            ~== M.fromCols (m,n)
                    [ V.scale e x
                    | (e,x) <- zip (V.elems s) (M.cols a) ]
  where
    (m,n) = M.dim a
    _ = typed t a
    
prop_negate t x =
    M.negate x === M.map negate x
  where
    _ = typed t x

prop_conjugate t x =
    M.conjugate x === M.map conjugate x
  where
    _ = typed t x


-------------------------- Linear Algebra --------------------------

prop_trans t a =
    M.trans a
        ===
        M.update (M.zero (swap $ M.dim a)) [ (swap ij, e) | (ij,e) <- M.assocs a ]
  where
    swap (i,j) = (j,i)
    _ = typed t a
    
prop_conjTrans t a =
    M.conjTrans a === M.conjugate (M.trans a)
  where
    _ = typed t a

prop_rank1Update t alpha a =
    forAll (Test.vector m) $ \x ->
    forAll (Test.vector n) $ \y -> let y' = V.conjugate y in
        M.rank1Update alpha x y a
            ~==
            M.update (M.zero (m,n))
                     [ ((i,j), alpha * V.at x i * V.at y' j + e)
                     | ((i,j),e) <- M.assocs a
                     ]
  where
    (m,n)= M.dim a
    _ = typed t a

data MulMatrixAddVector e =
    MulMatrixAddVector Trans (Matrix e) (Vector e) (Vector e) deriving (Show)
    
instance (Storable e, Arbitrary e) => Arbitrary (MulMatrixAddVector e) where
    arbitrary = do
        transa <- arbitrary
        a <- arbitrary
        let (ma,na) = M.dim a
            (m,n) = case transa of NoTrans -> (ma,na)
                                   _       -> (na,ma)
        x <- Test.vector n
        y <- Test.vector m
        return $ MulMatrixAddVector transa a x y
        
data MulMatrixVector e =
    MulMatrixVector Trans (Matrix e) (Vector e) deriving (Show)
instance (Storable e, Arbitrary e) => Arbitrary (MulMatrixVector e) where
    arbitrary = do
        (MulMatrixAddVector transa a x _) <- arbitrary
        return $ MulMatrixVector transa a x
        
prop_mulVector t (MulMatrixVector transa a x) =
    M.mulVector transa a x
        ~==
        case transa of
            NoTrans   -> V.fromList (fst $ M.dim a)
                                    [ V.dot x (V.conjugate r)
                                    | r <- M.rows a ]

            Trans     -> V.fromList (snd $ M.dim a)
                                    [ V.dot x (V.conjugate c)
                                    | c <- M.cols a ]
                                    
            ConjTrans -> V.fromList (snd $ M.dim a)
                                    [ V.dot x c
                                    | c <- M.cols a ]
  where
    _ = typed t a

prop_mulVectorWithScale t alpha (MulMatrixVector transa a x) =
    M.mulVectorWithScale alpha transa a x
        ~==
        M.mulVector transa a (V.scale alpha x)
  where
    _ = typed t a

prop_addMulVectorWithScales t alpha beta (MulMatrixAddVector transa a x y) =
    M.addMulVectorWithScales alpha transa a x beta y
        ~==
        V.add (M.mulVectorWithScale alpha transa a x)
              (V.scale beta y)
  where
    _ = typed t a

data MulMatrixAddMatrix e =
    MulMatrixAddMatrix Trans (Matrix e) Trans (Matrix e) (Matrix e) deriving (Show)
    
instance (Storable e, Arbitrary e) => Arbitrary (MulMatrixAddMatrix e) where
    arbitrary = do
        transa <- arbitrary
        transb <- arbitrary
        c <- arbitrary
        k <- fst `fmap` Test.dim2
        
        let (m,n) = M.dim c
            (ma,na) = case transa of NoTrans -> (m,k)
                                     _       -> (k,m)
            (mb,nb) = case transb of NoTrans -> (k,n)
                                     _       -> (n,k)
        a <- Test.matrix (ma,na)
        b <- Test.matrix (mb,nb)
        
        return $ MulMatrixAddMatrix transa a transb b c

data MulMatrixMatrix e =
    MulMatrixMatrix Trans (Matrix e) Trans (Matrix e) deriving (Show)
instance (Storable e, Arbitrary e) => Arbitrary (MulMatrixMatrix e) where
    arbitrary = do
        (MulMatrixAddMatrix transa a transb b _) <- arbitrary
        return $ MulMatrixMatrix transa a transb b

prop_mulMatrix t (MulMatrixMatrix transa a transb b) =
    M.mulMatrix transa a transb b
        ~==
        M.fromCols (m,n) [ M.mulVector transa a x | x <- M.cols b' ]
  where
    m = case transa of NoTrans -> (fst $ M.dim a)
                       _       -> (snd $ M.dim a)
    n = case transb of NoTrans -> (snd $ M.dim b)
                       _       -> (fst $ M.dim b)
    b' = case transb of NoTrans   -> b
                        Trans     -> M.trans b
                        ConjTrans -> M.conjTrans b
    _ = typed t a

prop_mulMatrixWithScale t alpha (MulMatrixMatrix transa a transb b) =
    M.mulMatrixWithScale alpha transa a transb b
        ~==
        M.scale alpha (M.mulMatrix transa a transb b)
  where
    _ = typed t a

prop_addMulMatrixWithScales t alpha beta (MulMatrixAddMatrix transa a transb b c) =
    M.addMulMatrixWithScales alpha transa a transb b beta c
        ~==
        M.add (M.scale alpha (M.mulMatrix transa a transb b))
              (M.scale beta c)
  where
    _ = typed t a



testAEq a b =
    if a ~== b then True
               else trace ("expected: " ++ show b ++ "\nactual: " ++ show a) False
