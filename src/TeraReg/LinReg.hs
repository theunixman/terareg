module TeraReg.LinReg where

import Numeric.LinearAlgebra.HMatrix

-- Is this right? Maybe...
ols x y = (pinv (tr x) `mul` x ) `mul` (tr x `mul` y)
