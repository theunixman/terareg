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
        q11 x = subMatrix (0, 0) ((x_h x) `div` 2, (x_w x) `div` 2) x
        q12 x = subMatrix (0, (x_w x) `div` 2 + 1) ((x_h x `div` 2), x_w x - ((x_w x) `div` 2)) x
        q21 x = subMatrix ((x_h x) `div` 2 + 1, 0) (x_h x - (x_h x) `div` 2, (x_w x) `div` 2) x
        q22 x = subMatrix ((x_h x) `div` 2 + 1, (x_w x) `div` 2 + 1) (x_h x - (x_h x) `div` 2, x_w x - ((x_w x) `div` 2)) x

        strass mm nn = do
            [a, b, c, d, e, f, g, h] <- mapConcurrently (\(m0, m1) -> strassen m0 m1) [
                ((q11 mm), (q11 nn)),
                ((q12 mm), (q21 nn)),

                ((q11 mm), (q12 nn)),
                ((q12 mm), (q22 nn)),

                ((q21 mm), (q12 nn)),
                ((q22 mm), (q22 nn)),

                ((q21 mm), (q12 nn)),
                ((q22 mm), (q22 nn))]

            let c11 = a + b
            let c12 = c + d
            let c21 = e + f
            let c22 = g + h

            return ((c11 ||| c12) === (c21 ||| c22))

-- The quad size in words (8 bytes each word)
quadwords :: Int
quadwords = 128 * 1024 * 1024

