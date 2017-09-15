module Main exposing (main)

import AnimationFrame
import Camera
import Environment
import Geometry
import Html
import Navigator
import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (vec3)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Projection
import SkyDome
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
        navigator =
            Navigator.init (vec2 2000 2000)
    in
        ( { canvasSize = Projection.defaultWindowSize
          , projectionMatrix = Projection.makeProjection Projection.defaultWindowSize
          , camera = Camera.set (vec3 2000 Geometry.cameraHeight 2000) 0
          , environment = Environment.init
          , navigator = navigator
          , skyDome = SkyDome.init
          , terrain = Terrain.init
          , water = Water.init
          , errorMessage = Nothing
          }
        , Cmd.batch
            [ Task.perform WindowSize Window.size
            , Navigator.runTileQueries navigator
            ]
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes WindowSize
        , AnimationFrame.diffs (Animate << Time.inSeconds)
        ]
