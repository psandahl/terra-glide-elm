module SkySphere exposing (SkySphere, init)

import Math.Vector3 exposing (Vec3)
import SkySphere.IcoSphere as IS
import WebGL as GL
import WebGL exposing (Mesh)


type alias SkySphere =
    { mesh : Mesh Vertex
    }


type alias Vertex =
    { position : Vec3
    }


init : SkySphere
init =
    { mesh = GL.triangles <| List.map toVertex <| IS.icosphere 3
    }


toVertex : ( Vec3, Vec3, Vec3 ) -> ( Vertex, Vertex, Vertex )
toVertex ( v1, v2, v3 ) =
    ( { position = v1 }, { position = v2 }, { position = v3 } )
