module Types exposing (Model, Msg(..))

import Http
import Math.Matrix4 exposing (Mat4)
import Terrain exposing (Terrain)
import Terrain.TileData exposing (TileData)
import Window exposing (Size)


{-| Application state.
-}
type alias Model =
    { canvasSize : Size
    , projectionMatrix : Mat4
    , terrain : Terrain
    , errorMessage : Maybe String
    }


{-| Application messages.
-}
type
    Msg
    -- The window size has changed.
    = WindowSize Size
      -- The execution of a TileQuery has resulted in new TileData.
    | NewTileData ( Int, Int ) (Result Http.Error TileData)
