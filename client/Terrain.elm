module Terrain
    exposing
        ( Terrain
        , init
        , addTile
        , entities
        )

import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as Mat
import Msg exposing (Msg(..))
import Task
import Terrain.TileData exposing (TileData)
import Terrain.Tile exposing (Tile)
import Terrain.Tile as Tile
import WebGL exposing (Entity)
import WebGL.Texture as Texture
import WebGL.Texture exposing (Texture)


type alias Terrain =
    { tiles : List Tile
    }


init : Terrain
init =
    { tiles = []
    }


addTile : ( Int, Int ) -> TileData -> Terrain -> Terrain
addTile pos tileData terrain =
    { terrain | tiles = Tile.init pos tileData :: terrain.tiles }


entities : Mat4 -> Mat4 -> Terrain -> List Entity
entities projectionMatrix viewMatrix terrain =
    List.map
        (Tile.toEntity viewMatrix <|
            Mat.mul projectionMatrix viewMatrix
        )
        terrain.tiles
