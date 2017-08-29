module Perlin
    ( PerlinContext (..)
    , WorldQuery (..)
    , defaultPerlinContext
    , asHeightmap
    , module Perlin.Algorithm
    ) where

import           Codec.Picture        (Image, PixelRGB8 (..), encodePng,
                                       generateImage)
import           Data.ByteString.Lazy (ByteString)
import           Perlin.Algorithm
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
        , weights = [ (1, 1)
                    , (3, 0.5)
                    , (6, 0.25)
                    , (25, 0.015)
                    ]
        , permute = init
        }

-- | Generate a heightmap as a PNG image encoded to a 'ByteString'.
asHeightmap :: PerlinContext -> WorldQuery -> ByteString
asHeightmap context =
    encodePng . perlinImage context

perlinImage :: PerlinContext -> WorldQuery -> Image PixelRGB8
perlinImage context worldQuery =
    generateImage
        (\x z ->
            let xFrac = fromIntegral (xPos worldQuery + x) / fromIntegral (widthDividend context)
                zFrac = fromIntegral (zPos worldQuery + z) / fromIntegral (depthDividend context)
                y     = composedNoise2D (permute context) xFrac zFrac (weights context)
            in toColor y
        ) (width worldQuery) (depth worldQuery)

toColor :: Double -> PixelRGB8
toColor value =
    let value' = normalizeToFloat value
        color = round $ value' * 255
    in PixelRGB8 color color color
