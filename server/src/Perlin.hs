module Perlin
    ( PerlinContext (..)
    , WorldQuery (..)
    , Mesh (..)
    , Vertex (..)
    , defaultPerlinContext
    , asHeightmapPng
    , asHeightMapR16
    , asMesh
    , generateRaw16
    , generateMesh
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
import           Perlin.Mesh          (Mesh (..), Vertex (..), generateIndices,
                                       generateMesh)
import           Perlin.Raw16         (generateRaw16, toWord16)
import           Prelude              hiding (init)

-- | A context with parameters for generating a mesh or image.
data PerlinContext = PerlinContext
    { depthDividend :: !Int
    , widthDividend :: !Int
    , weights       :: ![(Double, Double)]
    , permute       :: !Permute
    } deriving Show

data WorldQuery = WorldQuery
    { xPos       :: !Int
    , zPos       :: !Int
    , worldWidth :: !Int
    , worldDepth :: !Int
    , yScale     :: !Int
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
asHeightmapPng :: PerlinContext -> WorldQuery -> ByteString
asHeightmapPng context worldQuery =
    encodePng $ generateImage (\x -> toColor . perlin context worldQuery x)
                              (worldWidth worldQuery) (worldDepth worldQuery)

-- | Generate a heightmap as a R16 bitmap encoded to a 'ByteString'.
asHeightMapR16 :: PerlinContext -> WorldQuery -> ByteString
asHeightMapR16 context worldQuery =
    generateRaw16 (\x -> toWord16 . perlin context worldQuery x)
                  (worldWidth worldQuery) (worldDepth worldQuery)

-- | Generate a Mesh. No encoding.
asMesh :: PerlinContext -> WorldQuery -> Mesh
asMesh context worldQuery =
    generateMesh (perlin context worldQuery)
                 (worldWidth worldQuery)
                 (worldDepth worldQuery)

-- | Workhorse function. From the context and a pair of coordinates
-- (starting at 0, 0) produce a V3, where x and z are moved using the offset
-- from the WorldQuery and y is scaled according to the WorldQuery.
perlin :: PerlinContext -> WorldQuery -> Int -> Int -> V3 Float
perlin context worldQuery x z =
    let xOffset = xPos worldQuery + x
        zOffset = zPos worldQuery + z
        xFrac   = fromIntegral xOffset / fromIntegral (widthDividend context)
        zFrac   = fromIntegral zOffset / fromIntegral (depthDividend context)
        y       = composedNoise2D (permute context) xFrac zFrac (weights context)
        yScaled = fromIntegral (yScale worldQuery) * normalizeToFloat y
    in V3 (fromIntegral xOffset) yScaled (fromIntegral zOffset)

toColor :: V3 Float -> PixelRGB8
toColor (V3 _x y _z) =
    let color = round y
    in PixelRGB8 color color color
