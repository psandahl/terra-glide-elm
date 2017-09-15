module Terrain
    exposing
        ( Terrain
        , init
        , addTile
        , entities
        )

import Geometry
import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as Mat
import Terrain.TileData exposing (TileData)
import Terrain.Tile exposing (Tile)
import Terrain.Tile as Tile
import WebGL exposing (Entity)


type alias Terrain =
    { tiles : List Tile
    , indices : List ( Int, Int, Int )
    }


init : Terrain
init =
    { tiles = []
    , indices = generateIndices (Geometry.tileSize + 1) -- +1 is glue between tiles
    }


addTile : ( Int, Int ) -> TileData -> Terrain -> Terrain
addTile pos tileData terrain =
    { terrain | tiles = Tile.init pos terrain.indices tileData :: terrain.tiles }


entities : Mat4 -> Mat4 -> Terrain -> List Entity
entities projectionMatrix viewMatrix terrain =
    List.map
        (Tile.toEntity viewMatrix <|
            Mat.mul projectionMatrix viewMatrix
        )
        terrain.tiles


generateIndices : Int -> List ( Int, Int, Int )
generateIndices tileSize =
    List.concatMap
        (\row ->
            List.concatMap
                (\col ->
                    let
                        upperLeft =
                            row * tileSize + col

                        upperRight =
                            upperLeft + 1

                        lowerLeft =
                            (row + 1) * tileSize + col

                        lowerRight =
                            lowerLeft + 1
                    in
                        [ ( upperRight, upperLeft, lowerLeft )
                        , ( upperRight, lowerLeft, lowerRight )
                        ]
                )
            <|
                List.range 0 (tileSize - 2)
        )
    <|
        List.range 0 (tileSize - 2)
