module Terrain exposing (Terrain, init, entities)

import WebGL exposing (Entity)


type alias Terrain =
    { dummy : Int
    }


init : Terrain
init =
    { dummy = 1 }


entities : Terrain -> List Entity
entities terrain =
    []
