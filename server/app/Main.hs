{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import           Data.Aeson         (encode)
import           Data.Text.Lazy     (Text)
import           Network.HTTP.Types (badRequest400)
import           Perlin             (PerlinContext, WorldQuery (..))
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
        get "/terrain/heightmap/mesh" $ heightmapMesh perlin `rescue` badRequest

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
    worldQuery <- worldQueryConstantScale 255
    setHeader "Content-Type" "image/png"
    raw $ Perlin.asHeightmapPng perlin worldQuery

heightmapR16 :: PerlinContext -> ActionM ()
heightmapR16 perlin = do
    worldQuery <- worldQueryConstantScale 65535
    setHeader "Content-Type" "image/r16"
    raw $ Perlin.asHeightMapR16 perlin worldQuery

heightmapMesh :: PerlinContext -> ActionM ()
heightmapMesh perlin = do
    worldQuery <- worldQueryWithScale
    setHeader "Content-Type" "application/json; charset=utf-8"
    raw $ encode (Perlin.asMesh perlin worldQuery)

worldQueryConstantScale :: Int -> ActionM WorldQuery
worldQueryConstantScale scale =
    WorldQuery <$> param "xpos" <*> param "zpos"
               <*> param "width" <*> param "depth"
               <*> pure scale

worldQueryWithScale :: ActionM WorldQuery
worldQueryWithScale =
    WorldQuery <$> param "xpos" <*> param "zpos"
               <*> param "width" <*> param "depth"
               <*> param "yscale"

badRequest :: Text -> ActionM ()
badRequest msg = do
    status badRequest400
    text msg
