{-# LANGUAGE BangPatterns #-}

module DataGen where

import qualified Data.ByteString as B
import Data.Serialize
import Data.Word
import Numeric.LinearAlgebra.HMatrix
import Unsafe.Coerce

-- S1. THE ONLY USES OF unsafeCoerce ARE HERE. Do not use it further
-- without a conversation with MOC.

doubleToWord64 :: Double -> Word64
doubleToWord64 = unsafeCoerce

word64ToDouble :: Word64 -> Double
word64ToDouble = unsafeCoerce

-- END S1 --

zeroVec :: Int -> Vector Double
zeroVec n = vector (replicate n 0.0)

flagMatrix :: Int -> Double -> Matrix Double
flagMatrix n cov =
  build (n, n) $ \i j -> if i == j then 1 else cov

generatePredictors :: Seed -> Int -> Int -> Double -> Matrix Double
generatePredictors seed nPoints nDim cov =
  gaussianSample seed nPoints (zeroVec nDim) (flagMatrix nDim cov)

generateResponses :: Seed -> Vector Double -> Double -> Matrix Double -> Vector Double
generateResponses seed weights noise predictors =
  noiseVec + (vector $ map (\xs -> dot weights xs) (toRows predictors))
  where noiseVec = (randomVector seed Gaussian n) * vector [noise]
        (n, _)   = size predictors

data Config = Config {seed1   :: Seed,
                      seed2   :: Seed,
                      nPoints :: Int,
                      weights :: Vector Double,
                      noise   :: Double,
                      cov     :: Double} deriving Show

defaultConfig n p = Config 867 5309 n w 0.25 0.99
  where w = build p (\i -> if (i == 0) then 1 else 0)

generateData :: Config -> (Matrix Double, Vector Double)
generateData (Config s1 s2 n wts nx cov') = 
  let predictors = generatePredictors s1 n (size wts) cov'
      responses  = generateResponses s2 wts nx predictors
  in (predictors, responses)

doublesToBytes :: [Double] -> B.ByteString
doublesToBytes doubles =
  encode $ map doubleToWord64 doubles

bytesToDoubles :: B.ByteString -> [Double]
bytesToDoubles bytes =
  case decode bytes of
    Left str    -> error str
    Right words -> map word64ToDouble words

--TODO: if our Vectors get too large for memory, we'll use
--Data.ByteString.Lazy instead. 
    
writeVector :: Vector Double -> String -> IO ()
writeVector vec filename =
  (B.writeFile filename . doublesToBytes . toList) vec

writeMatrix :: Matrix Double -> String -> IO ()
writeMatrix mat filename =
  (B.writeFile filename . doublesToBytes . toList . flatten) mat

readVector :: String -> IO (Vector Double)
readVector filename =
  fmap (fromList . bytesToDoubles) (B.readFile filename)

readMatrix :: String -> Int -> IO (Matrix Double)
readMatrix filename nRow =
  fmap (matrix nRow . bytesToDoubles) (B.readFile filename)

inverseTest :: IO ()
inverseTest = do
  let (predictors, responses) = generateData (defaultConfig 15 10)
  writeMatrix predictors "X.dat"
  writeVector responses  "y.dat"
  mat <- readMatrix "X.dat" 10
  vec <- readVector "y.dat"
  if mat == predictors && vec == responses then
    putStrLn "Passed inverse test."
  else
    putStrLn "Failed inverse test."
