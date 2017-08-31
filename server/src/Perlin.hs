module Perlin
    ( PerlinContext (..)
    , WorldQuery (..)
    , defaultPerlinContext
    , asHeightmapPng
    , asHeightMapR16
    , generateRaw16
    , module Perlin.Algorithm
    ) where

import           Codec.Picture        (PixelRGB8 (..), encodePng, generateImage)
import           Data.ByteString.Lazy (ByteString)
import           Perlin.Algorithm
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
    { xPos  :: !Int
    , zPos  :: !Int
    , width :: !Int
    , depth :: !Int
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
                              (width worldQuery) (depth worldQuery)

-- | Generate a heightmap as a R16 bitmap encoded to a 'ByteString'.
asHeightMapR16 :: PerlinContext -> WorldQuery -> ByteString
asHeightMapR16 context worldQuery =
    generateRaw16 (\x -> toWord16 . perlin context worldQuery x)
                  (width worldQuery) (depth worldQuery)

-- | Workhorse function. From all type of context and a pair of coordinates
-- (starting at 0, 0) produce a normalized Float value.
perlin :: PerlinContext -> WorldQuery -> Int -> Int -> Float
perlin context worldQuery x z =
    let xFrac = fromIntegral (xPos worldQuery + x) / fromIntegral (widthDividend context)
        zFrac = fromIntegral (zPos worldQuery + z) / fromIntegral (depthDividend context)
        y     = composedNoise2D (permute context) xFrac zFrac (weights context)
    in normalizeToFloat y

toColor :: Float -> PixelRGB8
toColor value =
    let color = round $ value * 255
    in PixelRGB8 color color color
