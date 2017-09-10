module Msg exposing (Msg(..))

import Http
import Terrain.TileData exposing (TileData)
import WebGL.Texture as Texture
import WebGL.Texture exposing (Texture)
import Window exposing (Size)


{-| Application messages.
-}
type
    Msg
    -- The window size has changed.
    = WindowSize Size
      -- The execution of a TileQuery has resulted in new TileData.
    | NewTileData ( Int, Int ) (Result Http.Error TileData)
      -- Animation request, with the (fraction of) seconds since last
      -- animation frame.
    | Animate Float
      -- The Terrain's request for the dirt texture is done.
    | DirtTexture (Result Texture.Error Texture)
      -- The Terrain's request for the grass texture is done.
    | GrassTexture (Result Texture.Error Texture)
      -- The Terrain's request for the rock texture is done.
    | RockTexture (Result Texture.Error Texture)
      -- The Terrain's request for the snow texture is done.
    | SnowTexture (Result Texture.Error Texture)
