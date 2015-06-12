module TeraReg.LinReg where

import Numeric.LinearAlgebra.HMatrix
import TeraReg.MatrixMult (strassen)

ols :: 
    (Field t, Numeric t) =>
    Matrix t -> Vector t -> Vector t
ols x y = (pinv ((tr x) `mul` x )) #> ((tr x) #> y)

iols :: 
    Numeric t =>
    Vector t -> Vector t -> t
iols x b = x <·> b

fast_ols :: Matrix Double -> Vector Double -> IO (Vector Double)
fast_ols x y = do
    i0 <- strassen (tr x) x
    return $ (pinv i0) #> ((tr x) #> y)
