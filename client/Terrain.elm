module Terrain
    exposing
        ( Terrain
        , init
        , addTile
        , addTileQueries
        , purgePassedTiles
        , entities
        )

import Environment exposing (Environment)
import Geometry
import Math.Vector2 exposing (Vec2)
import Math.Vector2 as Vec2
import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as Mat
import Terrain.TileData exposing (TileData)
import Terrain.Tile exposing (Tile)
import Terrain.Tile as Tile
import Terrain.TileQuery exposing (TileQuery)
import WebGL exposing (Entity)


type alias Terrain =
    { tiles : List Tile
    , tileQueries : List TileQuery
    , indices : List ( Int, Int, Int )
    }


init : Terrain
init =
    { tiles = []
    , tileQueries = []
    , indices = generateIndices (Geometry.tileSize + 1) -- +1 is glue between tiles
    }


addTile : ( Int, Int ) -> TileData -> Terrain -> Terrain
addTile pos tileData terrain =
    { terrain | tiles = Tile.init pos terrain.indices tileData :: terrain.tiles }


addTileQueries : List TileQuery -> Terrain -> ( List TileQuery, Terrain )
addTileQueries tileQueries terrain =
    let
        acceptedQueries =
            List.filter (not << haveMatchingTileQuery terrain) tileQueries

        newTerrain =
            { terrain | tileQueries = terrain.tileQueries ++ acceptedQueries }
    in
        ( acceptedQueries, newTerrain )


purgePassedTiles : Vec2 -> Terrain -> Terrain
purgePassedTiles position terrain =
    let
        keptTiles =
            List.filter (keepTile <| tileStart position) terrain.tiles

        keptTileQueries =
            List.filter (keepTileQuery <| tileStart position) terrain.tileQueries
    in
        { terrain | tiles = keptTiles, tileQueries = keptTileQueries }


haveMatchingTileQuery : Terrain -> TileQuery -> Bool
haveMatchingTileQuery terrain tileQuery =
    List.any
        (\tq ->
            tq.xPos == tileQuery.xPos && tq.zPos == tileQuery.zPos
        )
        terrain.tileQueries


entities : Mat4 -> Mat4 -> Environment -> Terrain -> List Entity
entities projectionMatrix viewMatrix environment terrain =
    List.map
        (Tile.toEntity viewMatrix
            (Mat.mul projectionMatrix viewMatrix)
            environment
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


keepTile : Int -> Tile -> Bool
keepTile startZ tile =
    tile.startZ >= startZ


keepTileQuery : Int -> TileQuery -> Bool
keepTileQuery startZ tileQuery =
    tileQuery.zPos >= startZ


tileStart : Vec2 -> Int
tileStart position =
    Geometry.tileSize * (floor (Vec2.getY position) // Geometry.tileSize)
