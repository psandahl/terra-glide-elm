module Terrain exposing (Terrain, init, addTile, addDirt, addGrass, entities)

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
    , grass : Maybe Texture
    }


init : ( Terrain, Cmd Msg )
init =
    ( { tiles = []
      , dirt = Nothing
      , grass = Nothing
      }
    , Cmd.batch
        [ Task.attempt DirtTexture <| Texture.load "/textures/dirt.png"
        , Task.attempt GrassTexture <| Texture.load "/textures/grass.png"
        ]
    )


addTile : ( Int, Int ) -> TileData -> Terrain -> Terrain
addTile pos tileData terrain =
    { terrain | tiles = Tile.init pos tileData :: terrain.tiles }


addDirt : Texture -> Terrain -> Terrain
addDirt dirt terrain =
    { terrain | dirt = Just dirt }


addGrass : Texture -> Terrain -> Terrain
addGrass grass terrain =
    { terrain | grass = Just grass }


entities : Mat4 -> Mat4 -> Terrain -> List Entity
entities projectionMatrix viewMatrix terrain =
    case Maybe.map2 (,) terrain.dirt terrain.grass of
        Just ( dirt, grass ) ->
            List.map
                (Tile.toEntity dirt grass viewMatrix <|
                    Mat.mul projectionMatrix viewMatrix
                )
                terrain.tiles

        Nothing ->
            []
