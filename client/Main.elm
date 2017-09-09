module Main exposing (main)

import Camera
import Html
import Navigator
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Projection
import Task
import Update
import View
import Window
import Terrain


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = Update.update
        , view = View.view
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    let
        ( navigator, navigatorCommands ) =
            Navigator.init (vec2 0 0)
    in
        ( { canvasSize = Projection.defaultWindowSize
          , projectionMatrix = Projection.makeProjection Projection.defaultWindowSize
          , camera = Camera.set (vec3 50 150 180) (vec2 0 -1)
          , terrain = Terrain.init
          , errorMessage = Nothing
          }
        , Cmd.batch [ Task.perform WindowSize Window.size, navigatorCommands ]
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes WindowSize
