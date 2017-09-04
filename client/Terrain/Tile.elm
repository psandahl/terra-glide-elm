module Terrain.Tile exposing (Tile, init)

import Terrain.TileData exposing (TileData)


type alias Tile =
    { startX : Int
    , startZ : Int
    , width : Int
    , depth : Int
    }


init : ( Int, Int ) -> TileData -> Tile
init ( startX, startZ ) tileData =
    { startX = startX
    , startZ = startZ
    , width = tileData.width
    , depth = tileData.depth
    }
