{-# Language FlexibleInstances #-}

module Tests (tests) where

import Distribution.TestSuite.QuickCheck
import Test.QuickCheck
import Numeric.LinearAlgebra.HMatrix 
import TeraReg.QuadTree
import TeraReg.MatrixMult
import TeraReg.DataGen

tests :: IO [Test]
tests = return [
    testProperty "Transpose" check_trans,
    testProperty "Strassen Quads" check_strassQuads
    ]

instance Arbitrary (Matrix Double) where
    arbitrary = return $ generatePredictors 8675309 10 10 0.25

check_strassQuads m =
    let
        (w1, h1) = size m
        r1 = (Rect 0 0 w1 h1)
        q1 = quadRect m $ fromRect r1
        n = tr m
        (w2, h2) = size n
        r2 = (Rect 0 0 w2 h2)
        q2 = quadRect n $ fromRect r2
    in
        (m `mul` n) ==  (q1 `strassQuads` q2)

check_trans :: Matrix Double -> Bool
check_trans m =
    let
        (w, h) = size m
        quad = fromRect $ Rect 0 0 w h
        (Quad trect _ _ _ _) = fmap transRect quad
        tmat = tr $ subMatrixFromRect m trect
    in
        tr m == tmat
