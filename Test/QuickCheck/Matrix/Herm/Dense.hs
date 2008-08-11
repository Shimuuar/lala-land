-----------------------------------------------------------------------------
-- |
-- Module     : Test.QuickCheck.Matrix.Herm.Dense
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module Test.QuickCheck.Matrix.Herm.Dense 
    where

import Test.QuickCheck hiding ( vector )
import qualified Test.QuickCheck as QC
import Test.QuickCheck.Vector.Dense ( dvector )
import Test.QuickCheck.Matrix ( matrixSized )
import Test.QuickCheck.Matrix.Dense ( dmatrix, rawMatrix )

import BLAS.Elem ( BLAS2 )
import BLAS.Types ( flipUpLo )

import Data.Vector.Dense
import Data.Matrix.Dense
import Data.Matrix.Herm



hermMatrix :: (BLAS2 e, Arbitrary e) => Int -> Gen (Matrix (n,n) e)
hermMatrix n  = do
    a <- rawMatrix (n,n)
    let h = (a + herm a)
    elements [ h, herm h ]


data HermMatrix n e = 
    HermMatrix (Herm Matrix (n,n) e)
               (Matrix (n,n) e)
    deriving Show

instance (Arbitrary e, BLAS2 e) => Arbitrary (HermMatrix n e) where
    arbitrary = matrixSized $ \k -> do
        n <- choose (0,k)
        a <- hermMatrix n
        
        junk <- QC.vector (n*n)
        let (u ,b ) = (Upper, a // zip (filter (uncurry (>)) $ indices a) junk)
            (u',b') = (Lower, a // zip (filter (uncurry (<)) $ indices a) junk)

        h <- elements [ fromBase u             b
                      , fromBase (flipUpLo u)  (herm b)
                      , fromBase u'            b'
                      , fromBase (flipUpLo u') (herm b')
                      ]
        return $ HermMatrix h a
        
    coarbitrary = undefined


data HermMatrixMV n e = 
    HermMatrixMV (Herm Matrix (n,n) e) 
                 (Matrix (n,n) e) 
                 (Vector n e) 
    deriving Show

instance (Arbitrary e, BLAS2 e) => Arbitrary (HermMatrixMV n e) where
    arbitrary = do
        (HermMatrix h a) <- arbitrary
        x <- dvector (numCols a)
        return $ HermMatrixMV h a x
        
    coarbitrary = undefined

    
data HermMatrixMM m n e = 
    HermMatrixMM (Herm Matrix (m,m) e) 
                 (Matrix (m,m) e) 
                 (Matrix (m,n) e) 
    deriving Show
    
instance (Arbitrary e, BLAS2 e) => Arbitrary (HermMatrixMM m n e) where
    arbitrary = matrixSized $ \k -> do
        (HermMatrix h a) <- arbitrary
        n <- choose (0,k)
        b <- dmatrix (numCols a,n)
        return $ HermMatrixMM h a b
            
    coarbitrary = undefined
        