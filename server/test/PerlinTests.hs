module PerlinTests
    ( rawFileGeneration
    , meshGenerationDimensions
    , meshGenerationContents
    , indexGeneration2x2
    , indexGeneration4x3
    ) where

import           Data.Binary.Put      (putWord16le, runPut)
import qualified Data.ByteString.Lazy as LBS
import           Data.Vector          ((!))
import qualified Data.Vector          as Vector
import           Data.Word            (Word16)
import           Linear.V2            (V2 (..))
import           Linear.V3            (V3 (..))
import           Perlin               (TileData (..), Vertex (..),
                                       generateIndices, generateRaw16,
                                       generateTileData)
import           Test.HUnit

-- | Check that the generated raw file has the expected content and size.
-- Generate a small file of width 3 and the height 2.
rawFileGeneration :: Assertion
rawFileGeneration =
    testDataRaw @=? generateRaw16 writeCoordRaw w h

-- | Check that the generated Mesh have the expected dimensions.
meshGenerationDimensions :: Assertion
meshGenerationDimensions = do
    let mesh = generateTileData writeCoordMesh w h
    width mesh @=? w
    depth mesh @=? h
    (w * h) @=? Vector.length (vertices mesh)

-- | Check that the generated Mesh have the expected contents.
meshGenerationContents :: Assertion
meshGenerationContents = do
    let mesh = generateTileData writeCoordMesh 2 2
        v1 = vertices mesh ! 0
        v2 = vertices mesh ! 1
        v3 = vertices mesh ! 2
        v4 = vertices mesh ! 3

    V3 0 0 0 @=? position v1
    V3 (-0.57735026) 0.57735026 (-0.57735026) @=? normal v1
    V2 0 1 @=? texCoord v1

    V3 1 1 0 @=? position v2
    V3 0 1 0 @=? normal v2
    V2 1 1 @=? texCoord v2

    V3 0 1 1 @=? position v3
    V3 0 1 0 @=? normal v3
    V2 0 0 @=? texCoord v3

    V3 1 0 1 @=? position v4
    V3 0.57735026 0.57735026 0.57735026 @=? normal v4
    V2 1 0 @=? texCoord v4

-- | Check that the simplest possible index generation look as expected.
indexGeneration2x2 :: Assertion
indexGeneration2x2 =
    Vector.fromList [1, 0, 2, 1, 2, 3] @=? generateIndices 2 2

-- | Check a more complex index generation.
indexGeneration4x3 :: Assertion
indexGeneration4x3 =
    Vector.fromList
        [ 1, 0, 4, 1, 4, 5
        , 2, 1, 5, 2, 5, 6
        , 3, 2, 6, 3, 6, 7
        , 5, 4, 8, 5, 8, 9
        , 6, 5, 9, 6, 9, 10
        , 7, 6, 10, 7, 10, 11
        ] @=? generateIndices 4 3

-- | The value for the coordinate is y * width + x ...
writeCoordRaw :: Int -> Int -> Word16
writeCoordRaw x y = fromIntegral $ y * w + x

writeCoordMesh :: Int -> Int -> V3 Float
writeCoordMesh 0 0 = V3 0 0 0
writeCoordMesh 1 0 = V3 1 1 0
writeCoordMesh 0 1 = V3 0 1 1
writeCoordMesh 1 1 = V3 1 0 1
writeCoordMesh x z = V3 (fromIntegral x) 0 (fromIntegral z)

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
