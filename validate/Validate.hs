import Numeric.LinearAlgebra.Data

import TeraReg.DataGen
import TeraReg.LinReg

ns :: Int
ns = 5000000

vs :: Int
vs = 100

w :: Int
w = 1000

main :: IO ()
main = do
    let (p, r) = generateData $ defaultConfig ns vs

    -- train
    let trainp = dropRows w p
    let trainr = subVector w (ns - w) r

    -- withheld ps, should be a list of vectors
    let wp = toRows $ takeRows w p :: [Vector Double]

    -- withheld rs, should be a list of doubles
    let wr = toList $ subVector 0 w r :: [Double]

    model <- fast_ols trainp trainr

    let mse = (sum $ map (\(tp, x) -> ((iols model tp) - x) ** 2) $ zip wp wr) / (fromIntegral w)

    print mse
