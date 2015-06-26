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

epsilon = 0.00001

instance Arbitrary (Matrix Double) where
    arbitrary = return $ generatePredictors 8675309 16 4 0.25

quadFromMatrix :: Matrix Double -> Quad (Matrix Double)
quadFromMatrix m =
    quadRect m q
    where
        (h, w) = size m
        q = fromRect $ Rect 0 0 w h
        
check_strassQuads m =
    let
        quad1 = quadFromMatrix m
        quad2 = quadFromMatrix $ tr m
        adm = abs $ (m `mul` (tr m)) -  (quad1 `strassQuads` quad2)
    in
        maxElement adm <= epsilon

check_trans :: Matrix Double -> Bool
check_trans m =
    let
        (w, h) = size m
        quad = fromRect $ Rect 0 0 w h
        (Quad trect _ _ _ _) = fmap transRect quad
        tmat = tr $ subMatrixFromRect m trect
    in
        tr m == tmat
