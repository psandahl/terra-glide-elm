module Navigator exposing (Navigator, init)

import Constants
import Math.Vector2 exposing (Vec2)
import Msg exposing (Msg)
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
    , Cmd.batch
        [ TileQuery.execute
            { xPos = 0
            , zPos = 0
            , tileWidth = Constants.tileSize
            , tileDepth = Constants.tileSize
            , yScale = 100
            }
        ]
    )
