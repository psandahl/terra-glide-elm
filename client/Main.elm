module Main exposing (main)

import AnimationFrame
import Camera
import Constants
import Html
import Navigator
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Projection
import Task
import Time
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
        ( terrain, terrainCommands ) =
            Terrain.init

        ( navigator, navigatorCommands ) =
            Navigator.init (vec2 10000 10000)
    in
        ( { canvasSize = Projection.defaultWindowSize
          , projectionMatrix = Projection.makeProjection Projection.defaultWindowSize
          , camera = Camera.set (vec3 10000 Constants.cameraHeight 10000) (vec2 (sin 0) (cos 0))
          , cameraRotation = 0
          , terrain = terrain
          , errorMessage = Nothing
          }
        , Cmd.batch [ Task.perform WindowSize Window.size, navigatorCommands, terrainCommands ]
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes WindowSize
        , AnimationFrame.diffs (Animate << Time.inSeconds)
        ]
