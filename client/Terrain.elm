module Terrain exposing (Terrain, init, addTile, entities)

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


entities : Terrain -> List Entity
entities terrain =
    []
