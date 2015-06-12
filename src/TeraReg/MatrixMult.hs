module TeraReg.MatrixMult where

import Control.Concurrent.Async (mapConcurrently)
import Numeric.LinearAlgebra.HMatrix hiding ((<>))

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

-- |A tree of rectangles into quadrants.
data QuadTree a =
    Node Int (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a) |
    Leaf Int a a a a
    deriving Show

quadTreeWords :: QuadTree t -> Int
quadTreeWords (Node i _ _ _ _) = i
quadTreeWords (Leaf i _ _ _ _) = i

data Rect = Rect {
    left :: Int,
    top :: Int,
    width :: Int,
    height :: Int
    } deriving Show

rectWords :: Rect -> Int
rectWords (Rect _ _ w h) = w * h

-- Calculate z-ordered coordinates for a matrix of height h and width w.
zorder :: Int -> Int -> Int -> QuadTree Rect
zorder wid hei block =
    zord $ Rect 0 0 wid hei
    where
        zord :: Rect -> QuadTree Rect
        zord z =
            if rectWords z <= block then
                newLeaf (q1 z) (q2 z) (q3 z) (q4 z)
            else
                newNode (zord $ q1 z) (zord $ q2 z) (zord $ q3 z) (zord $ q4 z)
            where
                newLeaf a b c d =
                    Leaf (sum $ map rectWords [a, b, c, d]) a b c d
                newNode a b c d =
                    Node (sum $ map quadTreeWords [a, b, c, d]) a b c d

        q1 (Rect l t w h) = Rect l t (w `div` 2) (h `div` 2)
        q2 (Rect l t w h) = Rect (l + w `div` 2) t (w - w `div` 2) (h `div` 2)
        q3 (Rect l t w h) = Rect l (t + h `div` 2) (w `div` 2) (h - h `div` 2)
        q4 (Rect l t w h) = Rect (l + w `div` 2) (t + h `div` 2) (w - w `div` 2) (h - h `div` 2)
