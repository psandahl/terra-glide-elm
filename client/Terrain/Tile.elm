module Terrain.Tile exposing (Tile, init, toEntity)

import Math.Vector3 exposing (Vec3)
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


toEntity : Mat4 -> Mat4 -> Tile -> Entity
toEntity viewMatrix mvpMatrix tile =
    GL.entity vertexShader
        fragmentShader
        tile.mesh
        { viewMatrix = viewMatrix
        , mvpMatrix = mvpMatrix
        }


tuplify : List ( Int, Int, Int ) -> List Int -> List ( Int, Int, Int )
tuplify tgt src =
    case List.take 3 src of
        [ a, b, c ] ->
            tuplify (( a, b, c ) :: tgt) (List.drop 3 src)

        _ ->
            List.reverse tgt


vertexShader :
    Shader Vertex
        { unif
            | viewMatrix : Mat4
            , mvpMatrix : Mat4
        }
        { vNormal : Vec3 }
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 position;
        attribute vec3 normal;
        attribute vec2 texCoord;

        uniform mat4 viewMatrix;
        uniform mat4 mvpMatrix;

        varying vec3 vNormal;

        void main()
        {
            vNormal = (viewMatrix * vec4(normal, 0.0)).xyz;
            gl_Position = mvpMatrix * vec4(position, 1.0);
        }
    |]


fragmentShader : Shader {} { unif | viewMatrix : Mat4 } { vNormal : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;

        uniform mat4 viewMatrix;

        varying vec3 vNormal;

        // Ambient color stuff. Hardcoded for now.
        vec3 ambientColor = vec3(1.0, 1.0, 1.0);
        float ambientStrength = 0.2;

        // Diffuse color stuff. Hardcoded for now.
        vec3 diffuseColor = vec3(182.0/255.0, 126.0/255.0, 91.0/255.0);

        // Calculate the base color for the fragment.
        vec3 baseColor();

        // Get the sun's direction. In view space.
        vec3 sunDirection();

        // Calculate the ambient light component.
        vec3 calcAmbientLight();

        // Calculate the diffuse light component.
        vec3 calcDiffuseLight();

        void main()
        {
            vec3 fragmentColor = baseColor() *
                (calcAmbientLight() + calcDiffuseLight());
            gl_FragColor = vec4(fragmentColor, 1.0);
        }

        vec3 baseColor()
        {
            return vec3(0.0, 1.0, 0.0);
        }

        vec3 sunDirection()
        {
            // To the east.
            vec4 direction = viewMatrix * vec4(1.0, 1.0, 0.0, 0.0);
            return normalize(direction.xyz);
        }

        vec3 calcAmbientLight()
        {
            return ambientColor * ambientStrength;
        }

        vec3 calcDiffuseLight()
        {
            vec3 normal = normalize(vNormal);
            float diffuse = max(dot(normal, sunDirection()), 0.0);
            return diffuseColor * diffuse;
        }
    |]
