module Terrain.Tile exposing (Tile, init, toEntity)

import Debug
import Environment exposing (Environment)
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
toEntity : Mat4 -> Mat4 -> Environment -> Tile -> Entity
toEntity viewMatrix mvpMatrix environment tile =
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
        , ambientColor = environment.ambientColor
        , ambientStrength = environment.ambientStrength
        , diffuseColor = environment.diffuseColor
        , sunDirection = environment.sunDirection
        , lowerTerrainLower = environment.lowerTerrain.lower
        , lowerTerrainUpper = environment.lowerTerrain.upper
        , upperTerrainLower = environment.upperTerrain.lower
        , upperTerrainUpper = environment.upperTerrain.upper
        , fogColor = environment.fogColor
        , fogDistance = environment.fogDistance
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
        , vTransformedPosition : Vec3
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
        varying vec3 vTransformedPosition;
        varying vec3 vTransformedNormal;

        void main()
        {
            vPosition = position;
            vTransformedPosition = (viewMatrix * vec4(position, 1.0)).xyz;
            vTransformedNormal = (viewMatrix * vec4(normal, 0.0)).xyz;
            gl_Position = mvpMatrix * vec4(position, 1.0);
        }
    |]


fragmentShader :
    Shader {}
        { unif
            | viewMatrix : Mat4
            , terrainHeight : Float
            , ambientColor : Vec3
            , ambientStrength : Float
            , diffuseColor : Vec3
            , sunDirection : Vec3
            , lowerTerrainLower : Vec3
            , lowerTerrainUpper : Vec3
            , upperTerrainLower : Vec3
            , upperTerrainUpper : Vec3
            , fogColor : Vec3
            , fogDistance : Float
        }
        { vPosition : Vec3
        , vTransformedPosition : Vec3
        , vTransformedNormal : Vec3
        }
fragmentShader =
    [glsl|
        precision mediump float;

        uniform mat4 viewMatrix;
        uniform float terrainHeight;
        uniform vec3 ambientColor;
        uniform float ambientStrength;
        uniform vec3 diffuseColor;
        uniform vec3 sunDirection;
        uniform vec3 lowerTerrainLower;
        uniform vec3 lowerTerrainUpper;
        uniform vec3 upperTerrainLower;
        uniform vec3 upperTerrainUpper;
        uniform vec3 fogColor;
        uniform float fogDistance;

        varying vec3 vPosition;
        varying vec3 vTransformedPosition;
        varying vec3 vTransformedNormal;

        // Calculate the texture color for the fragment.
        vec3 baseColor();

        // Get the light's direction. Transformed to view space.
        vec3 lightDirection();

        // Calculate the ambient light component.
        vec3 calcAmbientLight();

        // Calculate the diffuse light component.
        vec3 calcDiffuseLight();

        void main()
        {
            // Calculate the fragment color before applying fog.
            vec3 fragmentColor = baseColor() *
                (calcAmbientLight() + calcDiffuseLight());

            // Apply a linear fog.
            float dist = distance(vec3(0.0), vTransformedPosition);
            vec3 mixedWithFog = mix(fragmentColor, fogColor, smoothstep(0.0, fogDistance, dist));

            gl_FragColor = vec4(mixedWithFog, 1.0);
        }

        vec3 baseColor()
        {
            float y = vPosition.y / terrainHeight;

            vec3 color = mix(lowerTerrainLower, lowerTerrainUpper, smoothstep(0.0, 0.20, y));
            color = mix(color, upperTerrainLower, smoothstep(0.20, 0.7, y));
            return mix(color, upperTerrainUpper, smoothstep(0.7, 1.0, y));
        }

        vec3 lightDirection()
        {
            vec4 direction = viewMatrix * vec4(sunDirection, 0.0);
            return normalize(direction.xyz);
        }

        vec3 calcAmbientLight()
        {
            return ambientColor * ambientStrength;
        }

        vec3 calcDiffuseLight()
        {
            vec3 normal = normalize(vTransformedNormal);
            float diffuse = max(dot(normal, lightDirection()), 0.0);
            return diffuseColor * diffuse;
        }
    |]
