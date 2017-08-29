{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import           Data.Text.Lazy     (Text)
import           Network.HTTP.Types (badRequest400)
import           Perlin             (PerlinContext, WorldQuery (..))
import qualified Perlin
import           Web.Scotty

main :: IO ()
main = do
    let perlin = Perlin.defaultPerlinContext
    scotty 8000 $ do
        get "/world/heightmap/png" $ heightmapPng perlin `rescue` badRequest
        get "/world/heightmap/r16" $ heightmapR16 perlin `rescue` badRequest

heightmapPng :: PerlinContext -> ActionM ()
heightmapPng perlin = do
    worldQuery <- worldQueryParams
    setHeader "Content-Type" "image/png"
    raw $ Perlin.asHeightmapPng perlin worldQuery

heightmapR16 :: PerlinContext -> ActionM ()
heightmapR16 perlin = do
    worldQuery <- worldQueryParams
    setHeader "Content-Type" "image/r16"
    raw $ Perlin.asHeightMapR16 perlin worldQuery

worldQueryParams :: ActionM WorldQuery
worldQueryParams =
    WorldQuery <$> param "xpos" <*> param "zpos"
               <*> param "width" <*> param "depth"

badRequest :: Text -> ActionM ()
badRequest msg = do
    status badRequest400
    text msg
