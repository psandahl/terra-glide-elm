module Terrain.Tile exposing (Tile, init, toEntity)

import Debug
import Geometry
import Math.Vector3 exposing (Vec3, getY)
import Math.Matrix4 exposing (Mat4)
import Terrain.TileData exposing (Vertex, TileData)
import WebGL exposing (Entity, Mesh, Shader)
import WebGL as GL
import WebGL.Settings as Settings
import WebGL.Settings.DepthTest as DepthTest


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
init : ( Int, Int ) -> List ( Int, Int, Int ) -> TileData -> Tile
init ( startX, startZ ) indices tileData =
    { startX = startX
    , startZ = startZ
    , width = tileData.width
    , depth = tileData.depth
    , mesh = GL.indexedTriangles tileData.vertices indices
    }


checkHeights : List Vertex -> List Vertex
checkHeights xs =
    List.map
        (\v ->
            let
                y =
                    getY <| v.position
            in
                if y > Geometry.terrainHeight then
                    Debug.log "Error: > maxHeight" v
                else
                    v
        )
        xs


checkIndices : List ( Int, Int, Int ) -> List ( Int, Int, Int ) -> List ( Int, Int, Int )
checkIndices fromServer fromLocal =
    let
        deb =
            Debug.log "(server, local): " ( List.length fromServer, List.length fromLocal )
    in
        fromLocal


{-| Render the Tile.
-}
toEntity : Mat4 -> Mat4 -> Tile -> Entity
toEntity viewMatrix mvpMatrix tile =
    GL.entityWith
        [ DepthTest.default
        , Settings.cullFace Settings.back
        ]
        vertexShader
        fragmentShader
        tile.mesh
        { viewMatrix = viewMatrix
        , mvpMatrix = mvpMatrix
        , terrainHeight = Geometry.terrainHeight
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
        , vTransformedNormal : Vec3
        }
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 position;
        attribute vec3 normal;

        uniform mat4 viewMatrix;
        uniform mat4 mvpMatrix;

        varying vec3 vPosition;
        varying vec3 vTransformedNormal;

        void main()
        {
            vPosition = position;
            vTransformedNormal = (viewMatrix * vec4(normal, 0.0)).xyz;
            gl_Position = mvpMatrix * vec4(position, 1.0);
        }
    |]


fragmentShader :
    Shader {}
        { unif
            | viewMatrix : Mat4
            , terrainHeight : Float
        }
        { vPosition : Vec3
        , vTransformedNormal : Vec3
        }
fragmentShader =
    [glsl|
        precision mediump float;

        uniform mat4 viewMatrix;
        uniform float terrainHeight;

        varying vec3 vPosition;
        varying vec3 vTransformedNormal;

        // Ambient color stuff. Hardcoded for now.
        vec3 ambientColor = vec3(1.0, 1.0, 1.0);
        float ambientStrength = 0.2;

        // Diffuse color stuff. Hardcoded for now.
        vec3 diffuseColor = vec3(0.8, 0.8, 0.8);

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
            float y = vPosition.y / terrainHeight;

            vec3 lowerBand1 = vec3(239.0 / 255.0, 141.0 / 255.0, 55.0 / 255.0);
            vec3 upperBand1 = vec3(0.0, 1.0, 0.0);

            vec3 lowerBand2 = vec3(55.0 / 255.0, 68.0 / 255.0, 71.0 / 255.0);
            vec3 upperBand2 = vec3(1.0, 1.0, 1.0);

            vec3 color = mix(lowerBand1, upperBand1, smoothstep(0.0, 0.33, y));
            color = mix(color, lowerBand2, smoothstep(0.33, 0.66, y));
            return mix(color, upperBand2, smoothstep(0.66, 1.0, y));
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
