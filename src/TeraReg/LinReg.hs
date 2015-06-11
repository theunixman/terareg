module TeraReg.LinReg where

import Numeric.LinearAlgebra.HMatrix

ols ::
    (Field t, Numeric t) =>
    Matrix t -> Matrix t -> Matrix t
ols x y = (pinv ((tr x) `mul` x )) `mul` ((tr x) `mul` y)

iols ::
    Numeric t =>
    Matrix t -> Matrix t -> Matrix t
iols x b = (tr x) `mul` b
