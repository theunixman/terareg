module TeraReg.LinReg where

import Numeric.LinearAlgebra.HMatrix

ols :: 
    (Field t, Numeric t) =>
    Matrix t -> Vector t -> Vector t
ols x y = (pinv ((tr x) `mul` x )) #> ((tr x) #> y)

iols :: 
    Numeric t =>
    Vector t -> Vector t -> t
iols x b = x <·> b

thresh :: Int
thresh = 100000

fast_ols :: Matrix Double -> Vector Double -> IO (Vector Double)
fast_ols x y = do
    i0 <- strassen (tr x) x
    return $ (pinv i0) #> ((tr x) #> y)

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

        strass m n = do
            a <- ((q11 m) `strassen` (q11 n))
            b <- ((q12 m) `strassen` (q21 n))

            let c11 = a + b

            c <- ((q11 m) `strassen` (q12 n))
            d <- ((q12 m) `strassen` (q22 n))
            let c12 = c + d

            e <- ((q21 m) `strassen` (q12 n))
            f <- ((q22 m) `strassen` (q22 n))
            let c21 = e + f

            g <- ((q21 m) `strassen` (q12 n))
            h <- ((q22 m) `strassen` (q22 n))
            let c22 = g + h

            return ((c11 ||| c12) === (c21 ||| c22))

    
