module PerlinProps
    ( inRange
    , inNormalizedRange
    , isZeroIfInteger
    ) where

import qualified Perlin

-- | Check that the reply always is in range [-1, 1].
inRange :: Double -> Double -> Bool
inRange x z =
    let y = Perlin.noise2D Perlin.init x z
    in y >= -1 && y <= 1

-- | Check that a Double value always become in range [0, 1] when normalizeToFloat.
inNormalizedRange :: Double -> Bool
inNormalizedRange value =
    let y = Perlin.normalizeToFloat value
    in y >= 0 && y <= 1

-- | If the input are integers, the algorithm always produce zero.
isZeroIfInteger :: Int -> Int -> Bool
isZeroIfInteger x z =
    let y = Perlin.noise2D Perlin.init (fromIntegral x) (fromIntegral z)
    in y == 0.0
