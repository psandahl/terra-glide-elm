module Types exposing (Model, Msg(..), init)

import Http
import Terrain.MeshData exposing (MeshData)
import Window exposing (Size)


type alias Model =
    { canvasSize : Size
    }


type Msg
    = WindowSize Size
    | FetchMeshData ( Int, Int ) (Result Http.Error MeshData)


init : Model
init =
    { canvasSize = { width = 30, height = 30 } }
