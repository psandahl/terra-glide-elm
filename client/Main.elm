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
            Navigator.init (vec2 123456 0)

        terrain =
            Terrain.init
    in
        ( { canvasSize = Projection.defaultWindowSize
          , projectionMatrix = Projection.makeProjection Projection.defaultWindowSize
          , camera = Camera.set (vec3 123456 Geometry.cameraHeight 0) 0
          , environment = Environment.init
          , navigator = navigator
          , skyDome = SkyDome.init
          , terrain = terrain
          , water = Water.init
          , errorMessage = Nothing
          }
        , Cmd.batch
            [ Task.perform WindowSize Window.size
            , Navigator.runTileQueries terrain navigator
            ]
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes WindowSize
        , AnimationFrame.diffs Animate
        ]
