module Perlin
    ( PerlinContext (..)
    , TileQuery (..)
    , TileData (..)
    , Vertex (..)
    , defaultPerlinContext
    , asHeightmapPng
    , asHeightMapR16
    , asTileData
    , generateRaw16
    , generateTileData
    , generateIndices
    , init
    , noise2D
    , normalizeToFloat
    ) where

import           Codec.Picture        (PixelRGB8 (..), encodePng, generateImage)
import           Data.ByteString.Lazy (ByteString)
import           Linear.V3            (V3 (..))
import           Perlin.Algorithm     (Permute, composedNoise2D, init, noise2D,
                                       normalizeToFloat)
import           Perlin.Raw16         (generateRaw16, toWord16)
import           Perlin.Tile          (TileData (..), Vertex (..),
                                       generateIndices, generateTileData)
import           Prelude              hiding (init)

-- | A context with parameters for generating a mesh or image.
data PerlinContext = PerlinContext
    { depthDividend :: !Int
    , widthDividend :: !Int
    , weights       :: ![(Double, Double)]
    , permute       :: !Permute
    } deriving Show

-- | Query a tile somewhere in the terrain.
data TileQuery = TileQuery
    { xPos      :: !Int
    , zPos      :: !Int
    , tileWidth :: !Int
    , tileDepth :: !Int
    , yScale    :: !Int
    } deriving Show

-- | Generate a default 'PerlinContext'.
defaultPerlinContext :: PerlinContext
defaultPerlinContext =
    PerlinContext
        { depthDividend = 1051
        , widthDividend = 1051
        , weights = [ (1, 0.3)
                    , (1.3, 0.7)
                    , (2.1, 0.8)
                    , (2.2, 0.5)
                    , (2.4, 0.3)
                    , (2.4, 0.3)
                    , (15, 0.02)
                    , (17, 0.025)
                    , (19, 0.02)
                    , (21, 0.025)
                    , (30, 0.008)
                    , (33, 0.008)
                    , (36, 0.008)
                    , (40, 0.008)
                    , (41, 0.008)
                    , (45, 0.008)
                    , (133, 0.002)
                    , (166, 0.002)
                    ]
        , permute = init
        }

-- | Generate a heightmap as a PNG image encoded to a 'ByteString'.
asHeightmapPng :: PerlinContext -> TileQuery -> ByteString
asHeightmapPng context tileQuery =
    encodePng $ generateImage (\x -> toColor . perlin context tileQuery x)
                              (tileWidth tileQuery) (tileDepth tileQuery)

-- | Generate a heightmap as a R16 bitmap encoded to a 'ByteString'.
asHeightMapR16 :: PerlinContext -> TileQuery -> ByteString
asHeightMapR16 context tileQuery =
    generateRaw16 (\x -> toWord16 . perlin context tileQuery x)
                  (tileWidth tileQuery) (tileDepth tileQuery)

-- | Generate TileData. No encoding.
asTileData :: PerlinContext -> TileQuery -> TileData
asTileData context tileQuery =
    generateTileData (perlin context tileQuery)
                     (1 + tileWidth tileQuery)
                     (1 + tileDepth tileQuery)

-- | Workhorse function. From the context and a pair of coordinates
-- (starting at 0, 0) produce a V3, where x and z are moved using the offset
-- from the TileQuery and y is scaled according to the WorldQuery.
perlin :: PerlinContext -> TileQuery -> Int -> Int -> V3 Float
perlin context tileQuery x z =
    let xOffset = xPos tileQuery + x
        zOffset = zPos tileQuery + z
        xFrac   = fromIntegral xOffset / fromIntegral (widthDividend context)
        zFrac   = fromIntegral zOffset / fromIntegral (depthDividend context)
        y       = composedNoise2D (permute context) xFrac zFrac (weights context)
        yScaled = fromIntegral (yScale tileQuery) * normalizeToFloat y
    in V3 (fromIntegral xOffset) yScaled (fromIntegral zOffset)

toColor :: V3 Float -> PixelRGB8
toColor (V3 _x y _z) =
    let color = round y
    in PixelRGB8 color color color
