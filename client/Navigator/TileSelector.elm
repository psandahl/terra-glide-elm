module Navigator.TileSelector exposing (tiles)

import Debug
import Geometry
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector2 as Vec2
import Terrain.TileQuery exposing (TileQuery)


tiles : Vec2 -> List TileQuery
tiles point =
    let
        foo =
            Debug.log "tiles, point = " point

        ( xFarLeft, zFarLeft ) =
            tileStartFor <| farLeft point

        ( xFarRight, zFarRight ) =
            tileStartFor <| farRight point

        ( xNearLeft, zNearLeft ) =
            tileStartFor <| nearLeft point

        ( xNearRight, zNearRight ) =
            tileStartFor <| nearRight point
    in
        List.concatMap (tileRow xFarLeft xFarRight) <|
            List.range (zFarLeft // Geometry.tileSize) (zNearLeft // Geometry.tileSize)


tileRow : Int -> Int -> Int -> List TileQuery
tileRow firstX lastX z =
    List.map
        (\x ->
            { xPos = x * Geometry.tileSize
            , zPos = z * Geometry.tileSize
            , tileWidth = Geometry.tileSize
            , tileDepth = Geometry.tileSize
            , yScale = floor Geometry.terrainHeight
            }
        )
    <|
        List.range (firstX // Geometry.tileSize) (lastX // Geometry.tileSize)


tileStartFor : Vec2 -> ( Int, Int )
tileStartFor p =
    ( Geometry.tileSize * ((floor <| Vec2.getX p) // Geometry.tileSize)
    , Geometry.tileSize * ((floor <| Vec2.getY p) // Geometry.tileSize)
    )



-- In the below left is -x and right is x. Far is -z and near is z. I.e. it's
-- from a position facing the camera.


farLeft : Vec2 -> Vec2
farLeft p =
    let
        pp =
            vec2 -Geometry.tileVistaAside 0
    in
        Vec2.add p pp


farRight : Vec2 -> Vec2
farRight p =
    let
        pp =
            vec2 Geometry.tileVistaAside 0
    in
        Vec2.add p pp


nearLeft : Vec2 -> Vec2
nearLeft p =
    let
        pp =
            vec2 -Geometry.tileVistaAside Geometry.tileVistaAhead
    in
        Vec2.add p pp


nearRight : Vec2 -> Vec2
nearRight p =
    let
        pp =
            vec2 Geometry.tileVistaAside Geometry.tileVistaAhead
    in
        Vec2.add p pp
