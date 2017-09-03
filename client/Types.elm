module Types exposing (Model, Msg(..), init)

import Http
import Terrain.TileData exposing (TileData)
import Window exposing (Size)


type alias Model =
    { canvasSize : Size
    }


type Msg
    = WindowSize Size
    | FetchTileData ( Int, Int ) (Result Http.Error TileData)


init : Model
init =
    { canvasSize = { width = 30, height = 30 } }
