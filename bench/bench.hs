{-# Language OverloadedStrings #-}

import Criterion.Main hiding (defaultConfig)

import TeraReg.DataGen
import TeraReg.LinReg

main :: IO ()
main = do
    let (p, r) = generateData $ defaultConfig 10000000 100
    
    defaultMain [
        bgroup "LinReg" [bench "ols" $ nf (ols p) r]
        ]
