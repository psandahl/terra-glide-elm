module PerlinTests
    ( rawFileGeneration
    , meshGeneration
    ) where

import           Data.Binary.Put      (putWord16le, runPut)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector          as Vector
import           Data.Word            (Word16)
import           Linear.V3            (V3 (..))
import           Perlin               (Mesh (..), generateMesh, generateRaw16)
import           Test.HUnit

-- | Check that the generated raw file has the expected content and size.
-- Generate a small file of width 3 and the height 2.
rawFileGeneration :: Assertion
rawFileGeneration =
    testDataRaw @=? generateRaw16 writeCoordRaw w h

-- | Check that the generated Mesh have the expected dimensions.
meshGeneration :: Assertion
meshGeneration = do
    let mesh = generateMesh writeCoordMesh w h
    width mesh @=? w
    depth mesh @=? h
    (w * h) @=? Vector.length (vertices mesh)

-- | The value for the coordinate is y * width + x ...
writeCoordRaw :: Int -> Int -> Word16
writeCoordRaw x y = fromIntegral $ y * w + x

writeCoordMesh :: Int -> Int -> V3 Float
writeCoordMesh x z = V3 (fromIntegral x) 1 (fromIntegral z)

-- | So put values in the expected way in the test data.
testDataRaw :: LBS.ByteString
testDataRaw =
    runPut $ do
        putWord16le 0
        putWord16le 1
        putWord16le 2
        putWord16le 3
        putWord16le 4
        putWord16le 5

w :: Int
w = 3

h :: Int
h = 2
