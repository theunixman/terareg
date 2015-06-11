{-# LANGUAGE BangPatterns #-}

module Main where

import TeraReg.DataGen
import TeraReg.LinReg

main :: IO ()
main = do
    let (p, r) = generateData $ defaultConfig 100000 1000

    print p

    print ""

    print r
    
    let b = ols p r

    print b
    
