module Perlin
    ( ImageDimensions
    , WorldCoordinates
    , PerlinContext (..)
    , defaultPerlinContext
    , asPngFile
    , module Perlin.Algorithm
    ) where

import           Codec.Picture
import           Perlin.Algorithm
import           Prelude          hiding (init)

-- | Image dimensions (width, height).
type ImageDimensions = (Int, Int)

-- | World coordinates (x, z) where to start generate mesh/image.
type WorldCoordinates = (Int, Int)

-- | A context with parameters for generating a mesh or image.
data PerlinContext = PerlinContext
    { heightDividend :: !Int
    , widthDividend  :: !Int
    , weights        :: ![(Double, Double)]
    , permute        :: !Permute
    } deriving Show

-- | Generate a default 'PerlinContext'.
defaultPerlinContext :: PerlinContext
defaultPerlinContext =
    PerlinContext
        { heightDividend = 104729
        , widthDividend = 104729
        , weights = [ (104729 / 500, 0.8)
                    , (104729 / 250, 0.2)
                    , (104729 / 100, 0.1)
                    ]
        , permute = init
        }

-- | Generate a PNG file to the given file path.
asPngFile :: FilePath -> PerlinContext -> ImageDimensions -> WorldCoordinates -> IO ()
asPngFile filePath context dimensions =
    writePng filePath . perlinImage context dimensions

perlinImage :: PerlinContext -> ImageDimensions -> WorldCoordinates -> Image PixelRGB8
perlinImage context (width, height) (startX, startZ) =
    generateImage
        (\x z ->
            let xFrac = fromIntegral (startX + x) / fromIntegral (widthDividend context)
                zFrac = fromIntegral (startZ + z) / fromIntegral (heightDividend context)
                y     = composedNoise2D (permute context) xFrac zFrac (weights context)
            in toColor y
        ) width height

toColor :: Double -> PixelRGB8
toColor value =
    let value' = normalizeToFloat value
        color = floor $ value' * 255
    in PixelRGB8 color color color
