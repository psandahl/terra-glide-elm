module View exposing (view)

import Html exposing (Html)
import Html
import Html.Attributes as Attr
import Model exposing (Model)
import Msg exposing (Msg)
import SkySphere
import Terrain
import Water
import WebGL as GL


view : Model -> Html Msg
view model =
    let
        viewMatrix =
            model.camera.viewMatrix

        skySphereEntity =
            SkySphere.entity model.projectionMatrix viewMatrix model.skySphere

        terrainEntities =
            Terrain.entities model.projectionMatrix viewMatrix model.terrain

        waterEntity =
            Water.entity model.projectionMatrix viewMatrix model.water
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
                (skySphereEntity :: terrainEntities)

            --    ++ [ waterEntity ]
            ]
