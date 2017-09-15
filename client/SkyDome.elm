module SkyDome exposing (SkyDome, init, entity)

import Environment exposing (Environment)
import Math.Matrix4 as Mat
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import SkyDome.IcoSphere as IS
import WebGL as GL
import WebGL.Settings.DepthTest as DepthTest
import WebGL exposing (Mesh, Entity, Shader)


type alias SkyDome =
    { mesh : Mesh Vertex
    , modelMatrix : Mat4
    }


type alias Vertex =
    { position : Vec3
    }


init : SkyDome
init =
    { mesh = GL.triangles <| List.map toVertex <| IS.icosphere 3
    , modelMatrix = Mat.makeScale3 2 2 2
    }


entity : Mat4 -> Mat4 -> Environment -> SkyDome -> Entity
entity projectionMatrix viewMatrix environment skyDome =
    let
        mvpMatrix =
            Mat.mul projectionMatrix <|
                Mat.mul (modifyViewMatrix viewMatrix) skyDome.modelMatrix
    in
        GL.entityWith
            [ DepthTest.always { write = False, near = 0, far = 1 } ]
            vertexShader
            fragmentShader
            skyDome.mesh
            { mvpMatrix = mvpMatrix
            , skyColor = environment.skyColor
            , horizonColor = environment.horizonColor
            , fogColor = environment.fogColor
            , fogHeight = environment.fogHeight
            }


toVertex : ( Vec3, Vec3, Vec3 ) -> ( Vertex, Vertex, Vertex )
toVertex ( v1, v2, v3 ) =
    ( { position = v1 }, { position = v2 }, { position = v3 } )


modifyViewMatrix : Mat4 -> Mat4
modifyViewMatrix viewMatrix =
    let
        r =
            Mat.toRecord viewMatrix
    in
        Mat.fromRecord <| { r | m14 = 0, m24 = 0, m34 = 0 }


vertexShader : Shader Vertex { unif | mvpMatrix : Mat4 } { vPosition : Vec3 }
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 position;

        uniform mat4 mvpMatrix;

        varying vec3 vPosition;

        void main()
        {
            vPosition = position;
            gl_Position = mvpMatrix * vec4(position, 1.0);
        }
    |]


fragmentShader :
    Shader {}
        { unif
            | skyColor : Vec3
            , horizonColor : Vec3
            , fogColor : Vec3
            , fogHeight : Float
        }
        { vPosition : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;

        uniform vec3 skyColor;
        uniform vec3 horizonColor;
        uniform vec3 fogColor;
        uniform float fogHeight;

        varying vec3 vPosition;

        void main()
        {
            float y = abs(vPosition.y);
            vec3 sky = mix(horizonColor, skyColor, y);

            if (y > fogHeight)
            {
                gl_FragColor = vec4(skyColor, 1.0);
            }
            else
            {
                gl_FragColor = vec4(mix(fogColor, skyColor, smoothstep(0.0, fogHeight, y)), 1.0);
            }
        }
    |]
