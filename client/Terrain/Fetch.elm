module Terrain.Fetch exposing (TileQuery, fetchTileData)

import Http exposing (Request)
import Http
import Terrain.TileData as TileData
import Types exposing (Msg(..))


type alias TileQuery =
    { xPos : Int
    , zPos : Int
    , worldWidth : Int
    , worldDepth : Int
    , yScale : Int
    }


fetchTileData : TileQuery -> Cmd Msg
fetchTileData tileQuery =
    Http.send (FetchTileData ( tileQuery.xPos, tileQuery.zPos )) <|
        Http.get (toUrl tileQuery) TileData.decode


toUrl : TileQuery -> String
toUrl tailQuery =
    service ++ "?" ++ asString tailQuery


asString : TileQuery -> String
asString tailQuery =
    "xpos="
        ++ toString tailQuery.xPos
        ++ "&"
        ++ "zpos="
        ++ toString tailQuery.zPos
        ++ "&"
        ++ "width="
        ++ toString tailQuery.worldWidth
        ++ "&"
        ++ "depth="
        ++ toString tailQuery.worldDepth
        ++ "&"
        ++ "yscale="
        ++ toString tailQuery.yScale


service : String
service =
    "/terrain/heightmap/tile"
