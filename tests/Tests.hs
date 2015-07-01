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
    testProperty "Strassen" check_strassen,
    testProperty "Strassen Quads" check_strassQuads,
    testProperty "m X m^T" check_qtStrass

    ]

epsilon = 0.00001

seed = choose (-1000000000, 10000000000)

instance Arbitrary (Matrix Double) where
arbitrary = sized $ \n -> do
    s <- seed
    return $ generatePredictors s ((n + 1) * 2) ((n + 1) * 6) 0.25

quadFromMatrix :: Matrix Double -> Quad (Matrix Double)
quadFromMatrix m =
    quadRect m q
    where
        (h, w) = size m
        q = fromRect $ Rect 0 0 w h

check_strassen :: Matrix Double -> Matrix Double -> Bool
check_strassen m n =
    let
        mq = quadFromMatrix m
        nq = quadFromMatrix n
        adm = abs $ (m `mul` n) - (mq `strassQuads` nq)
    in
        maxElement adm <= epsilon
        
check_strassQuads :: Matrix Double -> Bool
check_strassQuads m =
    let
        quad1 = quadFromMatrix m
        quad2 = quadFromMatrix $ tr m
        adm = abs $ (m `mul` (tr m)) -  (quad1 `strassQuads` quad2)
    in
        maxElement adm <= epsilon

check_qtStrass :: Matrix Double -> Bool
check_qtStrass m =
    let
        adm = abs $ (m `mul` (tr m)) -  (qtStrass m)
    in
        maxElement adm <= epsilon
    
-- check_trans :: Matrix Double -> Bool
-- check_trans m =
--     let
--         (w, h) = size m
--         quad = fromRect $ Rect 0 0 w h
--         (Quad trect _ _ _ _) = fmap transRect quad
--         tmat = tr $ subMatrixFromRect m trect
--     in
--         tr m == tmat

-- check_TransStrass
