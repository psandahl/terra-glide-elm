module Terrain.MeshData exposing (MeshData, Vertex, decode)

import Json.Decode exposing (Decoder)
import Json.Decode as Dec
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)


{-| This record is the exact same type as produced by the server.
-}
type alias MeshData =
    { width : Int
    , depth : Int
    , vertices : List Vertex
    }


{-| This Vertex record is the exact same type as produced by the server.
-}
type alias Vertex =
    { position : Vec3
    , normal : Vec3
    , texCoord : Vec2
    }


decode : Decoder MeshData
decode =
    Dec.map3 MeshData
        (Dec.field "width" Dec.int)
        (Dec.field "depth" Dec.int)
        (Dec.field "vertices" <| Dec.list decodeVertex)


decodeVec2 : Decoder Vec2
decodeVec2 =
    Dec.map2 vec2
        (Dec.field "s" Dec.float)
        (Dec.field "t" Dec.float)


decodeVec3 : Decoder Vec3
decodeVec3 =
    Dec.map3 vec3
        (Dec.field "x" Dec.float)
        (Dec.field "y" Dec.float)
        (Dec.field "z" Dec.float)


decodeVertex : Decoder Vertex
decodeVertex =
    Dec.map3 Vertex
        (Dec.field "position" decodeVec3)
        (Dec.field "normal" decodeVec3)
        (Dec.field "texCoord" decodeVec2)
