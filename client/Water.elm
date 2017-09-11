module Water exposing (Water, init, entity)

import Constants
import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as Mat
import Math.Vector3 exposing (Vec3, vec3)
import WebGL as GL
import WebGL exposing (Mesh, Entity, Shader)


type alias Water =
    { mesh : Mesh Vertex
    }


type alias Vertex =
    { position : Vec3
    }


init : Water
init =
    { mesh = GL.indexedTriangles vertices indices }


entity : Mat4 -> Mat4 -> Water -> Entity
entity projectionMatrix viewMatrix water =
    GL.entity
        vertexShader
        fragmentShader
        water.mesh
        { mvpMatrix = Mat.mul projectionMatrix viewMatrix
        }


vertices : List Vertex
vertices =
    [ { position = vec3 0 Constants.waterHeight 0 }
    , { position = vec3 Constants.maxWorld Constants.waterHeight 0 }
    , { position = vec3 0 Constants.waterHeight Constants.maxWorld }
    , { position = vec3 Constants.maxWorld Constants.waterHeight Constants.maxWorld }
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


fragmentShader : Shader {} unif {}
fragmentShader =
    [glsl|
        precision mediump float;

        vec3 waterColor = vec3(0.0, 0.0, 1.0);

        void main()
        {
            gl_FragColor = vec4(waterColor, 1.0);
        }
    |]
