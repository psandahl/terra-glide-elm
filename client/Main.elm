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
            Navigator.init (vec2 7890600 1100)

        ( proposedQueries, terrain ) =
            Terrain.addTileQueries (Navigator.proposeTileQueries navigator) Terrain.init
    in
        ( { canvasSize = Projection.defaultWindowSize
          , countDown = 60
          , projectionMatrix = Projection.makeProjection Projection.defaultWindowSize
          , camera = Camera.set (vec3 7890600 Geometry.cameraHeight 1100) 0
          , environment = Environment.init
          , navigator = navigator
          , skyDome = SkyDome.init
          , terrain = terrain
          , water = Water.init
          , errorMessage = Nothing
          }
        , Cmd.batch
            [ Task.perform WindowSize Window.size
            , Navigator.runTileQueries proposedQueries
            ]
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes WindowSize
        , if model.countDown > 0 then
            Time.every Time.second CountDown
          else
            AnimationFrame.diffs Animate
        ]
