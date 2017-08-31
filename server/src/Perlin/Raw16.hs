module Perlin.Raw16
    ( generateRaw16
    , toWord16
    ) where

import           Control.Monad        (forM_)
import           Data.Binary.Put      (putWord16le, runPut)
import           Data.ByteString.Lazy (ByteString)
import           Data.Word            (Word16)

-- | Generate a r16 file with the given dimensions and the given producer
-- function.
generateRaw16 :: (Int -> Int -> Word16) -> Int -> Int -> ByteString
generateRaw16 g width depth =
    runPut $
        forM_ [0 .. depth - 1] $ \z ->
            forM_ [0 .. width - 1] $ \x ->
                putWord16le $ g x z

-- | Convert the Float to a Word16.
toWord16 :: Float -> Word16
toWord16 value =
    round $ value * 65535
