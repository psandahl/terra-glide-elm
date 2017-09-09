module Navigator.TileSelector exposing (tiles)

import Constants
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector2 as Vec2
import Terrain.TileQuery exposing (TileQuery)


tiles : Vec2 -> List TileQuery
tiles point =
    let
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
            List.range (zFarLeft // Constants.tileSize) (zNearLeft // Constants.tileSize)


tileRow : Int -> Int -> Int -> List TileQuery
tileRow firstX lastX z =
    List.map
        (\x ->
            { xPos = x * Constants.tileSize
            , zPos = z * Constants.tileSize
            , tileWidth = Constants.tileSize
            , tileDepth = Constants.tileSize
            , yScale = floor Constants.terrainHeight
            }
        )
    <|
        List.range (firstX // Constants.tileSize) (lastX // Constants.tileSize)


tileStartFor : Vec2 -> ( Int, Int )
tileStartFor p =
    ( Constants.tileSize * ((floor <| Vec2.getX p) // Constants.tileSize)
    , Constants.tileSize * ((floor <| Vec2.getY p) // Constants.tileSize)
    )


{-| The far left point that is reached by from current point given the vista.
-}
farLeft : Vec2 -> Vec2
farLeft p =
    let
        pp =
            vec2 -Constants.tileVista -Constants.tileVista
    in
        Vec2.add p pp


{-| The far right point that is reached by from current point given the vista.
-}
farRight : Vec2 -> Vec2
farRight p =
    let
        pp =
            vec2 Constants.tileVista -Constants.tileVista
    in
        Vec2.add p pp


{-| The near left point that is reached by from current point given the vista.
-}
nearLeft : Vec2 -> Vec2
nearLeft p =
    let
        pp =
            vec2 -Constants.tileVista Constants.tileVista
    in
        Vec2.add p pp


{-| The near right point that is reached by from current point given the vista.
-}
nearRight : Vec2 -> Vec2
nearRight p =
    let
        pp =
            vec2 Constants.tileVista Constants.tileVista
    in
        Vec2.add p pp
