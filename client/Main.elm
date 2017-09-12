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
import SkySphere
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
    in
        ( { canvasSize = Projection.defaultWindowSize
          , projectionMatrix = Projection.makeProjection Projection.defaultWindowSize
          , camera = Camera.set (vec3 1000 Constants.cameraHeight 1000) (vec2 (sin 0) (cos 0))
          , cameraRotation = 0
          , navigator = Navigator.init (vec2 1000 1000)
          , skySphere = SkySphere.init
          , terrain = terrain
          , water = Water.init
          , errorMessage = Nothing
          }
        , Cmd.batch [ Task.perform WindowSize Window.size, terrainCommands ]
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes WindowSize
        , AnimationFrame.diffs (Animate << Time.inSeconds)
        ]
