module Main exposing (main)

import Camera
import Html
import Math.Vector3 exposing (vec3)
import Projection
import Task
import Types exposing (Model, Msg(..))
import Types
import Update
import View
import Window
import Terrain
import Terrain.TileQuery as TileQuery


main : Program Never Model Msg
main =
    Html.program
        { init =
            ( init
            , Cmd.batch
                [ Task.perform WindowSize Window.size
                , TileQuery.execute
                    { xPos = 0
                    , zPos = 0
                    , tileWidth = 2
                    , tileDepth = 2
                    , yScale = 1
                    }
                ]
            )
        , update = Update.update
        , view = View.view
        , subscriptions = subscriptions
        }


init : Model
init =
    { canvasSize = Projection.defaultWindowSize
    , projectionMatrix = Projection.makeProjection Projection.defaultWindowSize
    , camera = Camera.init (vec3 0 5 10) (vec3 0 0 -1)
    , terrain = Terrain.init
    , errorMessage = Nothing
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes WindowSize
