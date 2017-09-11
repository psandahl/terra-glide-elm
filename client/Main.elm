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
import Water
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
            Navigator.init (vec2 123456 123456)
    in
        ( { canvasSize = Projection.defaultWindowSize
          , projectionMatrix = Projection.makeProjection Projection.defaultWindowSize
          , camera = Camera.set (vec3 123456 Constants.cameraHeight 123456) (vec2 (sin 0) (cos 0))
          , cameraRotation = 0
          , terrain = terrain
          , water = Water.init
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
