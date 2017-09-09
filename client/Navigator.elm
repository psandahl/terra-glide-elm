module Navigator exposing (Navigator, init)

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
init : Vec2 -> ( Navigator, Cmd Msg )
init position =
    ( { position = position }
    , Cmd.batch <| List.map TileQuery.execute <| TileSelector.tiles position
    )
