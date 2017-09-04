module Main exposing (main)

import Html
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
    { canvasSize = { width = 800, height = 600 }
    , terrain = Terrain.init
    , errorMessage = Nothing
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes WindowSize
