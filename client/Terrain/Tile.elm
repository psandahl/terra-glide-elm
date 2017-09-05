module Terrain.Tile exposing (Tile, init, toEntity)

import Math.Matrix4 exposing (Mat4)
import Terrain.TileData exposing (Vertex, TileData)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL as GL


{-| A terrain tile. Mesh with some metadata. The mesh is already in world space.
-}
type alias Tile =
    { startX : Int
    , startZ : Int
    , width : Int
    , depth : Int
    , mesh : Mesh Vertex
    }


{-| Initialize a tile from start position and tile data.
-}
init : ( Int, Int ) -> TileData -> Tile
init ( startX, startZ ) tileData =
    { startX = startX
    , startZ = startZ
    , width = tileData.width
    , depth = tileData.depth
    , mesh = GL.indexedTriangles tileData.vertices <| tuplify [] tileData.indices
    }


toEntity : Mat4 -> Tile -> Entity
toEntity mvp tile =
    GL.entity vertexShader fragmentShader tile.mesh { mvp = mvp }


tuplify : List ( Int, Int, Int ) -> List Int -> List ( Int, Int, Int )
tuplify tgt src =
    case List.take 3 src of
        [ a, b, c ] ->
            tuplify (( a, b, c ) :: tgt) (List.drop 3 src)

        _ ->
            List.reverse tgt


vertexShader : Shader Vertex { mvp : Mat4 } {}
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec2 texCoord;

        uniform mat4 mvp;

        void main()
        {
            gl_Position = mvp * vec4(position, 1.0);
        }
    |]


fragmentShader : Shader {} unif {}
fragmentShader =
    [glsl|
        precision mediump float;

        void main()
        {
            gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0);
        }
    |]
