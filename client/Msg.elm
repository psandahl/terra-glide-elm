module Msg exposing (Msg(..))

import Http
import Terrain.TileData exposing (TileData)
import Time exposing (Time)
import Window exposing (Size)


{-| Application messages.
-}
type
    Msg
    -- The window size has changed.
    = WindowSize Size
      -- The execution of a TileQuery has resulted in new TileData.
    | NewTileData ( Int, Int ) (Result Http.Error TileData)
      -- Animation request, with the time that have passed since last animation.
    | Animate Time
