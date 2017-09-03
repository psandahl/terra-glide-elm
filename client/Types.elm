module Types exposing (Model, Msg(..))

import Http
import Terrain exposing (Terrain)
import Terrain.TileData exposing (TileData)
import Window exposing (Size)


type alias Model =
    { canvasSize : Size
    , terrain : Terrain
    }


type Msg
    = WindowSize Size
    | FetchTileData ( Int, Int ) (Result Http.Error TileData)
