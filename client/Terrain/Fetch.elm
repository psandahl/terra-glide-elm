module Terrain.Fetch exposing (TerrainQuery, fetchMeshData)

import Http exposing (Request)
import Http
import Terrain.MeshData as MeshData
import Types exposing (Msg(..))


type alias TerrainQuery =
    { xPos : Int
    , zPos : Int
    , worldWidth : Int
    , worldDepth : Int
    , yScale : Int
    }


fetchMeshData : TerrainQuery -> Cmd Msg
fetchMeshData terrainQuery =
    Http.send (FetchMeshData ( terrainQuery.xPos, terrainQuery.zPos )) <|
        Http.get (toUrl terrainQuery) MeshData.decode


toUrl : TerrainQuery -> String
toUrl terrainQuery =
    service ++ "?" ++ asString terrainQuery


asString : TerrainQuery -> String
asString terrainQuery =
    "xpos="
        ++ toString terrainQuery.xPos
        ++ "&"
        ++ "zpos="
        ++ toString terrainQuery.zPos
        ++ "&"
        ++ "width="
        ++ toString terrainQuery.worldWidth
        ++ "&"
        ++ "depth="
        ++ toString terrainQuery.worldDepth
        ++ "&"
        ++ "yscale="
        ++ toString terrainQuery.yScale


service : String
service =
    "/terrain/heightmap/mesh"
