module TeraReg.LinReg where

import Numeric.LinearAlgebra.HMatrix

ols :: 
    (Field t, Numeric t) =>
    Matrix t -> Vector t -> Vector t
ols x y = (pinv ((tr x) `mul` x )) #> ((tr x) #> y)

iols ::
    Numeric t =>
    Matrix t -> Vector t -> Vector t
iols x b = (tr x) #> b
