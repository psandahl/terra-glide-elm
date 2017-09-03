{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import           Data.Aeson         (encode)
import           Data.Text.Lazy     (Text)
import           Network.HTTP.Types (badRequest400)
import           Perlin             (PerlinContext, TileQuery (..))
import qualified Perlin
import           System.EasyFile    ((</>))
import           Web.Scotty

main :: IO ()
main = do
    let perlin = Perlin.defaultPerlinContext
    scotty 8000 $ do
        -- Routes to the start page.
        get "/index.html" startPage
        get "/" $ redirect "/index.html"

        -- Serving JavaScript.
        get "/scripts/:file" $
            serveFile "scripts" "application/javascript; charset=utf-8" =<<
                param "file"

        -- Terrain generation APIs.
        get "/terrain/heightmap/png" $ heightmapPng perlin `rescue` badRequest
        get "/terrain/heightmap/r16" $ heightmapR16 perlin `rescue` badRequest
        get "/terrain/heightmap/tile" $ heightmapTileData perlin `rescue` badRequest

startPage :: ActionM ()
startPage = do
    setHeader "Content-Type" "text/html; charset=utf-8"
    file $ "site" </> "index.html"

serveFile :: FilePath -> Text -> FilePath -> ActionM ()
serveFile resDir contentType requestedFile = do
    setHeader "Content-Type" contentType
    file $ "site" </> resDir </> requestedFile

heightmapPng :: PerlinContext -> ActionM ()
heightmapPng perlin = do
    tileQuery <- tileQueryConstantScale 255
    setHeader "Content-Type" "image/png"
    raw $ Perlin.asHeightmapPng perlin tileQuery

heightmapR16 :: PerlinContext -> ActionM ()
heightmapR16 perlin = do
    tileQuery <- tileQueryConstantScale 65535
    setHeader "Content-Type" "image/r16"
    raw $ Perlin.asHeightMapR16 perlin tileQuery

heightmapTileData :: PerlinContext -> ActionM ()
heightmapTileData perlin = do
    tileQuery <- tileQueryWithScale
    setHeader "Content-Type" "application/json; charset=utf-8"
    raw $ encode (Perlin.asTileData perlin tileQuery)

tileQueryConstantScale :: Int -> ActionM TileQuery
tileQueryConstantScale scale =
    TileQuery <$> param "xpos" <*> param "zpos"
              <*> param "width" <*> param "depth"
              <*> pure scale

tileQueryWithScale :: ActionM TileQuery
tileQueryWithScale =
    TileQuery <$> param "xpos" <*> param "zpos"
              <*> param "width" <*> param "depth"
              <*> param "yscale"

badRequest :: Text -> ActionM ()
badRequest msg = do
    status badRequest400
    text msg
