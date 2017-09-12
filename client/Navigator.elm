module Navigator exposing (Navigator, init, runTileQueries)

import Math.Vector2 exposing (Vec2)
import Msg exposing (Msg)
import Navigator.TileSelector as TileSelector
import Terrain.TileQuery as TileQuery


type alias Navigator =
    { position : Vec2
    }


{-| Initialize the Navigator at the given position. Give back a Navigator and
a command to be executed.
-}
init : Vec2 -> Navigator
init position =
    { position = position }


runTileQueries : Navigator -> Cmd Msg
runTileQueries navigator =
    Cmd.batch <| List.map TileQuery.execute <| TileSelector.tiles navigator.position
