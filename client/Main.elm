module Main exposing (main)

import Html
import Task
import Types exposing (Model, Msg(..))
import Types
import Update
import View
import Window
import Terrain.Fetch exposing (fetchTileData)


main : Program Never Model Msg
main =
    Html.program
        { init =
            ( Types.init
            , Cmd.batch
                [ Task.perform WindowSize Window.size
                , fetchTileData { xPos = 0, zPos = 0, worldWidth = 2, worldDepth = 2, yScale = 1 }
                ]
            )
        , update = Update.update
        , view = View.view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes WindowSize
