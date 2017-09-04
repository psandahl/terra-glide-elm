module Terrain exposing (Terrain, init, addTile, entities)

import Math.Matrix4 exposing (Mat4)
import Terrain.TileData exposing (TileData)
import WebGL exposing (Entity)


type alias Terrain =
    { dummy : Int
    }


init : Terrain
init =
    { dummy = 1 }


addTile : ( Int, Int ) -> TileData -> Terrain -> Terrain
addTile pos tileData terrain =
    terrain


entities : Mat4 -> Mat4 -> Terrain -> List Entity
entities projectionMatrix viewMatrix terrain =
    []
