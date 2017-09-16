module Navigator exposing (Navigator, init, animate, runTileQueries)

import Math.Vector2 as Vec2
import Math.Vector2 exposing (Vec2, vec2)
import Msg exposing (Msg)
import Navigator.TileSelector as TileSelector
import Time
import Time exposing (Time)
import Terrain exposing (Terrain)
import Terrain
import Terrain.TileQuery exposing (TileQuery)
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


animate : Time -> Navigator -> Navigator
animate time navigator =
    let
        newPosition =
            Vec2.add navigator.position <|
                Vec2.scale (Time.inSeconds time * speed) moveDirection
    in
        { navigator | position = newPosition }


runTileQueries : Terrain -> Navigator -> Cmd Msg
runTileQueries terrain navigator =
    Cmd.batch <|
        List.map TileQuery.execute <|
            removeAlreadyExistingTileQueries terrain <|
                TileSelector.tiles navigator.position


removeAlreadyExistingTileQueries : Terrain -> List TileQuery -> List TileQuery
removeAlreadyExistingTileQueries terrain tileQueries =
    List.filter (not << Terrain.haveMatchingTile terrain) tileQueries


speed : Float
speed =
    5


moveDirection : Vec2
moveDirection =
    vec2 0 1
