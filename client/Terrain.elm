module Terrain exposing (Terrain, init, addTile, addDirt, entities)

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
    , dirt : Maybe Texture
    }


init : ( Terrain, Cmd Msg )
init =
    ( { tiles = []
      , dirt = Nothing
      }
    , Task.attempt DirtTexture <| Texture.load "/textures/dirt.png"
    )


addTile : ( Int, Int ) -> TileData -> Terrain -> Terrain
addTile pos tileData terrain =
    { terrain | tiles = Tile.init pos tileData :: terrain.tiles }


addDirt : Texture -> Terrain -> Terrain
addDirt dirt terrain =
    { terrain | dirt = Just dirt }


entities : Mat4 -> Mat4 -> Terrain -> List Entity
entities projectionMatrix viewMatrix terrain =
    case terrain.dirt of
        Just dirt ->
            List.map
                (Tile.toEntity dirt viewMatrix <|
                    Mat.mul projectionMatrix viewMatrix
                )
                terrain.tiles

        Nothing ->
            []
