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
    scotty 8000 $
        get "/world/heightmap" $ heightmap perlin `rescue` badRequest

heightmap :: PerlinContext -> ActionM ()
heightmap perlin = do
    worldQuery <- worldQueryParams
    setHeader "Content-Type" "image/png"
    raw $ Perlin.asHeightmap perlin worldQuery

worldQueryParams :: ActionM WorldQuery
worldQueryParams =
    WorldQuery <$> param "xpos" <*> param "zpos"
               <*> param "width" <*> param "depth"

badRequest :: Text -> ActionM ()
badRequest msg = do
    status badRequest400
    text msg
