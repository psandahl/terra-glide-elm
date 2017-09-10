module Terrain.Tile exposing (Tile, init, toEntity)

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Matrix4 exposing (Mat4)
import Terrain.TileData exposing (Vertex, TileData)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL as GL
import WebGL.Settings as Settings
import WebGL.Settings.DepthTest as DepthTest
import WebGL.Texture exposing (Texture)


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


{-| Render the Tile.
-}
toEntity : Texture -> Texture -> Texture -> Mat4 -> Mat4 -> Tile -> Entity
toEntity dirt grass rock viewMatrix mvpMatrix tile =
    GL.entityWith
        [ DepthTest.default
        , Settings.cullFace Settings.back
        ]
        vertexShader
        fragmentShader
        tile.mesh
        { viewMatrix = viewMatrix
        , mvpMatrix = mvpMatrix
        , dirt = dirt
        , grass = grass
        , rock = rock
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
        { vPosition : Vec3
        , vNormal : Vec3
        , vTransformedNormal : Vec3
        , vTexCoord : Vec2
        }
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 position;
        attribute vec3 normal;
        attribute vec2 texCoord;

        uniform mat4 viewMatrix;
        uniform mat4 mvpMatrix;

        varying vec3 vPosition;
        varying vec3 vNormal;
        varying vec3 vTransformedNormal;
        varying vec2 vTexCoord;

        void main()
        {
            vPosition = position;
            vNormal = normal;
            vTransformedNormal = (viewMatrix * vec4(normal, 0.0)).xyz;
            vTexCoord = texCoord;
            gl_Position = mvpMatrix * vec4(position, 1.0);
        }
    |]


fragmentShader :
    Shader {}
        { unif
            | viewMatrix : Mat4
            , dirt : Texture
            , grass : Texture
            , rock : Texture
        }
        { vPosition : Vec3
        , vNormal : Vec3
        , vTransformedNormal : Vec3
        , vTexCoord : Vec2
        }
fragmentShader =
    [glsl|
        precision mediump float;

        uniform mat4 viewMatrix;
        uniform sampler2D dirt;
        uniform sampler2D grass;
        uniform sampler2D rock;

        varying vec3 vPosition;
        varying vec3 vNormal;
        varying vec3 vTransformedNormal;
        varying vec2 vTexCoord;

        // Ambient color stuff. Hardcoded for now.
        vec3 ambientColor = vec3(1.0, 1.0, 1.0);
        float ambientStrength = 0.2;

        // Diffuse color stuff. Hardcoded for now.
        vec3 diffuseColor = vec3(126.0/255.0, 126.0/255.0, 126.0/255.0);

        // Calculate the texture color for the fragment.
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
            if (vPosition.y > 50.0)
            {
                if (vNormal.y > 0.9)
                {
                    return texture2D(grass, vTexCoord).rgb;
                }
                else
                {
                    //return vec3(101.0 / 255.0, 96.0 / 255.0, 94.0 / 255.0);
                    return texture2D(rock, vTexCoord).rgb;
                }
            }
            else
            {
                return texture2D(dirt, vTexCoord).rgb;
            }
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
            vec3 normal = normalize(vTransformedNormal);
            float diffuse = max(dot(normal, sunDirection()), 0.0);
            return diffuseColor * diffuse;
        }
    |]
