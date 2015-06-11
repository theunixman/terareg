{-# LANGUAGE BangPatterns #-}

module Main where

import TeraReg.DataGen

main :: IO ()
main = do
    let (p, r) = generateData $ defaultConfig 100000 1
    writeMatrix p "pred.dat"
    writeVector r "resp.dat"
    
