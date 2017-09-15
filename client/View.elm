module View exposing (view)

import Html exposing (Html)
import Html
import Html.Attributes as Attr
import Model exposing (Model)
import Msg exposing (Msg)
import SkyDome
import Terrain
import Water
import WebGL as GL


view : Model -> Html Msg
view model =
    -- The order in which stuff is rendered is important. First must the
    -- sky dome be rendered, as it is rendered without depth information.
    -- Then the terrain is rendered, and last the water.
    let
        viewMatrix =
            model.camera.viewMatrix

        skyDomeEntity =
            SkyDome.entity model.projectionMatrix viewMatrix model.environment model.skyDome

        terrainEntities =
            Terrain.entities model.projectionMatrix viewMatrix model.environment model.terrain

        waterEntity =
            Water.entity model.projectionMatrix viewMatrix model.environment model.water
    in
        Html.div
            []
            [ GL.toHtmlWith
                [ GL.antialias
                , GL.depth 1
                ]
                [ Attr.height model.canvasSize.height
                , Attr.width model.canvasSize.width
                ]
              <|
                (skyDomeEntity :: terrainEntities)
                    ++ [ waterEntity ]
            ]
