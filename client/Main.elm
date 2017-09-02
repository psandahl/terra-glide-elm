module Main exposing (main)

import Html
import Types exposing (Model, Msg)
import Types
import Update
import View


main : Program Never Model Msg
main =
    Html.program
        { init = ( Types.init, Cmd.none )
        , update = Update.update
        , view = View.view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
