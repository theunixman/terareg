{-# Language OverloadedStrings #-}

import Criterion.Main hiding (defaultConfig)

import TeraReg.DataGen
import TeraReg.LinReg
import Numeric.LinearAlgebra.Data (toRows)

main :: IO ()
main = do
    let (p, r) = generateData $ defaultConfig 1000000 100

    let _ = toRows p
    
    defaultMain [
        bgroup "LinReg" [bench "ols" $ nfIO $ fast_ols p r]
        ]
