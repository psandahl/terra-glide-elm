module Main exposing (main)

import Html
import Task
import Types exposing (Model, Msg(..))
import Types
import Update
import View
import Window


main : Program Never Model Msg
main =
    Html.program
        { init = ( Types.init, Task.perform WindowSize Window.size )
        , update = Update.update
        , view = View.view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes WindowSize
