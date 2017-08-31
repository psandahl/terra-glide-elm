module PerlinTests
    ( rawFileGeneration
    ) where

import           Data.Binary.Put      (putWord16le, runPut)
import qualified Data.ByteString.Lazy as LBS
import           Data.Word            (Word16)
import           Perlin               (generateRaw16)
import           Test.HUnit

-- | Check that the generated raw file has the expected content and size.
-- Generate a small file of width 3 and the height 2.
rawFileGeneration :: Assertion
rawFileGeneration =
    testData @=? generateRaw16 writeCoord width height

-- | The value for the coordinate is y * width + x ...
writeCoord :: Int -> Int -> Word16
writeCoord x y = fromIntegral $ y * width + x

-- | So put values in the expected way in the test data.
testData :: LBS.ByteString
testData =
    runPut $ do
        putWord16le 0
        putWord16le 1
        putWord16le 2
        putWord16le 3
        putWord16le 4
        putWord16le 5

width :: Int
width = 3

height :: Int
height = 2
