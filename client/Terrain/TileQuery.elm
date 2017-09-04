module Terrain.TileQuery exposing (TileQuery, execute)

import Http exposing (Request)
import Http
import Terrain.TileData as TileData
import Types exposing (Msg(..))


{-| A TileQuery using the parameters specified in the record.
-}
type alias TileQuery =
    { xPos : Int
    , zPos : Int
    , tileWidth : Int
    , tileDepth : Int
    , yScale : Int
    }


{-| Execute the TileQuery.
-}
execute : TileQuery -> Cmd Msg
execute tileQuery =
    Http.send (NewTileData ( tileQuery.xPos, tileQuery.zPos )) <|
        Http.get (toUrl tileQuery) TileData.decode


toUrl : TileQuery -> String
toUrl tailQuery =
    service ++ "?" ++ asString tailQuery


asString : TileQuery -> String
asString tileQuery =
    "xpos="
        ++ toString tileQuery.xPos
        ++ "&"
        ++ "zpos="
        ++ toString tileQuery.zPos
        ++ "&"
        ++ "width="
        ++ toString tileQuery.tileWidth
        ++ "&"
        ++ "depth="
        ++ toString tileQuery.tileDepth
        ++ "&"
        ++ "yscale="
        ++ toString tileQuery.yScale


service : String
service =
    "/terrain/heightmap/tile"
