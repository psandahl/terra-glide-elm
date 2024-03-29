module Water exposing (Water, init, entity)

import Environment exposing (Environment)
import Geometry
import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as Mat
import Math.Vector3 exposing (Vec3, vec3)
import WebGL as GL
import WebGL exposing (Mesh, Entity, Shader)
import WebGL.Settings as Settings
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest


type alias Water =
    { mesh : Mesh Vertex
    }


type alias Vertex =
    { position : Vec3
    }


init : Water
init =
    { mesh = GL.indexedTriangles vertices indices }


entity : Mat4 -> Mat4 -> Environment -> Water -> Entity
entity projectionMatrix viewMatrix environment water =
    GL.entityWith
        [ DepthTest.default
        , Settings.cullFace Settings.back
        , Blend.add Blend.one Blend.one
        ]
        vertexShader
        fragmentShader
        water.mesh
        { mvpMatrix = Mat.mul projectionMatrix viewMatrix
        , waterColor = environment.waterColor
        }


vertices : List Vertex
vertices =
    [ { position = vec3 0 Geometry.waterHeight 0 }
    , { position = vec3 Geometry.maxWorld Geometry.waterHeight 0 }
    , { position = vec3 0 Geometry.waterHeight Geometry.maxWorld }
    , { position = vec3 Geometry.maxWorld Geometry.waterHeight Geometry.maxWorld }
    ]


indices : List ( Int, Int, Int )
indices =
    [ ( 1, 0, 2 ), ( 1, 2, 3 ) ]


vertexShader : Shader Vertex { unif | mvpMatrix : Mat4 } {}
vertexShader =
    [glsl|
        precision mediump float;

        attribute vec3 position;

        uniform mat4 mvpMatrix;

        void main()
        {
            gl_Position = mvpMatrix * vec4(position, 1.0);
        }
    |]


fragmentShader : Shader {} { unif | waterColor : Vec3 } {}
fragmentShader =
    [glsl|
        precision mediump float;

        uniform vec3 waterColor;

        void main()
        {
            gl_FragColor = vec4(waterColor, 1.0);
        }
    |]
