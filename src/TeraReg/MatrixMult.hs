{-# Language RankNTypes, KindSignatures #-}

module TeraReg.MatrixMult where

import Control.Concurrent.Async (mapConcurrently)
import Numeric.LinearAlgebra.HMatrix 
import TeraReg.QuadTree
    
thresh :: Int
thresh = 5000

strassen :: Matrix Double -> Matrix Double -> IO (Matrix Double)
strassen m n =
    if (x_h m < thresh)
    then return $ m `mul` n
    else strass m n
    where
        x_h = fst . size
        x_w = snd . size
        r11 x = subMatrix (0, 0) ((x_h x) `div` 2, (x_w x) `div` 2) x
        r12 x = subMatrix (0, (x_w x) `div` 2 + 1) ((x_h x `div` 2), x_w x - ((x_w x) `div` 2)) x
        r21 x = subMatrix ((x_h x) `div` 2 + 1, 0) (x_h x - (x_h x) `div` 2, (x_w x) `div` 2) x
        r22 x = subMatrix ((x_h x) `div` 2 + 1, (x_w x) `div` 2 + 1) (x_h x - (x_h x) `div` 2, x_w x - ((x_w x) `div` 2)) x

        strass mm nn = do
            [a, b, c, d, e, f, g, h] <- mapConcurrently (\(m0, m1) -> strassen m0 m1) [
                ((r11 mm), (r11 nn)),
                ((r12 mm), (r21 nn)),

                ((r11 mm), (r12 nn)),
                ((r12 mm), (r22 nn)),

                ((r21 mm), (r12 nn)),
                ((r22 mm), (r22 nn)),

                ((r21 mm), (r12 nn)),
                ((r22 mm), (r22 nn))]

            let c11 = a + b
            let c12 = c + d
            let c21 = e + f
            let c22 = g + h

            return ((c11 ||| c12) === (c21 ||| c22))

-- The quad size in words (8 bytes each word)
quadwords :: Int
quadwords = 128 * 1024 * 1024

subMatrixFromRect :: 
    Element a =>
    Matrix a -> Rect -> Matrix a
subMatrixFromRect mm (Rect l t w h) =
    subMatrix (t, l) (h, w) mm

quadRect ::
    forall (f :: * -> *) a.
    (Element a, Functor f) =>
    Matrix a -> f Rect -> f (Matrix a)
quadRect mm quad =
    fmap (subMatrixFromRect mm) quad

-- \mathbf{M}_{1} := (\mathbf{A}_{1,1} + \mathbf{A}_{2,2}) (\mathbf{B}_{1,1} + \mathbf{B}_{2,2})
-- \mathbf{M}_{2} := (\mathbf{A}_{2,1} + \mathbf{A}_{2,2}) \mathbf{B}_{1,1}
-- \mathbf{M}_{3} := \mathbf{A}_{1,1} (\mathbf{B}_{1,2} - \mathbf{B}_{2,2})
-- \mathbf{M}_{4} := \mathbf{A}_{2,2} (\mathbf{B}_{2,1} - \mathbf{B}_{1,1})
-- \mathbf{M}_{5} := (\mathbf{A}_{1,1} + \mathbf{A}_{1,2}) \mathbf{B}_{2,2}
-- \mathbf{M}_{6} := (\mathbf{A}_{2,1} - \mathbf{A}_{1,1}) (\mathbf{B}_{1,1} + \mathbf{B}_{1,2})
-- \mathbf{M}_{7} := (\mathbf{A}_{1,2} - \mathbf{A}_{2,2}) (\mathbf{B}_{2,1} + \mathbf{B}_{2,2})
--
-- only using 7 multiplications (one for each Mk) instead of 8. We may
-- now express the Ci,j in terms of Mk, like this:
--
-- 
-- \mathbf{C}_{1,1} = \mathbf{M}_{1} + \mathbf{M}_{4} - \mathbf{M}_{5} + \mathbf{M}_{7}
-- \mathbf{C}_{1,2} = \mathbf{M}_{3} + \mathbf{M}_{5}
-- \mathbf{C}_{2,1} = \mathbf{M}_{2} + \mathbf{M}_{4}
-- \mathbf{C}_{2,2} = \mathbf{M}_{1} - \mathbf{M}_{2} + \mathbf{M}_{3} + \mathbf{M}_{6}
strassQuads ::
    Quad (Matrix Double) ->
    Quad (Matrix Double) ->
    Matrix Double
strassQuads (Quad a11 a12 a21 a22) (Quad b11 b12 b21 b22) =
    let
        m1 = (a11 + a22) `mul` (b11 + b22)
        m2 = (a21 + a22) `mul` b11
        m3 = a11 `mul` (b12 - b22)
        m4 = a22 `mul` (b21 - b11)
        m5 = (a11 + a12) `mul` b22
        m6 = (a21 - a11) `mul` (b11 + b12)
        m7 = (a12 - a22) `mul` (b21 + b22)
        c11 = m1 + m4 - m5 + m7
        c12 = m3 + m5
        c21 = m2 + m4
        c22 = m1 - m2 + m3 + m6
    in
        (c11 ||| c12) === (c21 ||| c22)

qtStrass :: Matrix Double -> Matrix Double
qtStrass m =
    let
        (h, w) = size m
        strass (Leaf q) =
            let
                s = fmap (subMatrixFromRect m) q
                t = fmap tr s
            in
                strassQuads s t
        strass (Node a b c d) =
            let
                m11 = strass a
                m12 = strass b
                m21 = strass c
                m22 = strass d
            in
                (m11 ||| m12) === (m21 ||| m22)
        
        -- strass n@(Node a11 a12 a21 a22) =
        --     Leaf $ (m11 ||| m12) === (m21 ||| m22)
        --     where
        --         (Node m11 m12 m21 m22) = fmap strass n
        -- strass q = fmap strass q
            -- in
            --     
    in
        strass $ zorder (Rect 0 0 w h) quadwords
-- qtStrassen m n =
--     let
--         qt x = zorder $ Rect 0 0 (fst . size $ x) (snd . size $ x)
--         mqt = qt m
--         nqt = qt n
--         strass (Leaf (Quad a11 a12 a21 a22) (Leaf (Quad b11 b12 b21 b22)) = 
                        
--                         let a = ((q11 mm), (q11 nn)),
--                 ((q12 mm), (q21 nn)),

--                 ((q11 mm), (q12 nn)),
--                 ((q12 mm), (q22 nn)),

--                 ((q21 mm), (q12 nn)),
--                 ((q22 mm), (q22 nn)),

--                 ((q21 mm), (q12 nn)),
--                 ((q22 mm), (q22 nn))]

--             let c11 = a + b
--             let c12 = c + d
--             let c21 = e + f
--             let c22 = g + h

--             return ((c11 ||| c12) === (c21 ||| c22))
                        
--     in
        
