module Terrain
    exposing
        ( Terrain
        , init
        , addTile
        , addDirt
        , addGrass
        , addRock
        , addSnow
        , haveAllTextures
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
    , dirt : Maybe Texture
    , grass : Maybe Texture
    , rock : Maybe Texture
    , snow : Maybe Texture
    }


init : ( Terrain, Cmd Msg )
init =
    ( { tiles = []
      , dirt = Nothing
      , grass = Nothing
      , rock = Nothing
      , snow = Nothing
      }
    , Cmd.batch
        [ Task.attempt DirtTexture <| Texture.load "/textures/dirt.png"
        , Task.attempt GrassTexture <| Texture.load "/textures/grass.png"
        , Task.attempt RockTexture <| Texture.load "/textures/rock.png"
        , Task.attempt SnowTexture <| Texture.load "/textures/snow.png"
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


addRock : Texture -> Terrain -> Terrain
addRock rock terrain =
    { terrain | rock = Just rock }


addSnow : Texture -> Terrain -> Terrain
addSnow snow terrain =
    { terrain | snow = Just snow }


haveAllTextures : Terrain -> Bool
haveAllTextures terrain =
    case Maybe.map4 (,,,) terrain.dirt terrain.grass terrain.rock terrain.snow of
        Just _ ->
            True

        Nothing ->
            False


entities : Mat4 -> Mat4 -> Terrain -> List Entity
entities projectionMatrix viewMatrix terrain =
    case Maybe.map4 (,,,) terrain.dirt terrain.grass terrain.rock terrain.snow of
        Just ( dirt, grass, rock, snow ) ->
            List.map
                (Tile.toEntity dirt grass rock snow viewMatrix <|
                    Mat.mul projectionMatrix viewMatrix
                )
                terrain.tiles

        Nothing ->
            []
