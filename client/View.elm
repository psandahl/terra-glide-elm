module View exposing (view)

import Html exposing (Html)
import Html
import Html.Attributes as Attr
import Model exposing (Model)
import Msg exposing (Msg)
import Terrain
import WebGL as GL


view : Model -> Html Msg
view model =
    let
        viewMatrix =
            model.camera.viewMatrix
    in
        Html.div
            []
            [ GL.toHtmlWith
                [ GL.antialias
                , GL.depth 1
                , GL.clearColor (161 / 255) (187 / 255) (251 / 255) 0
                ]
                [ Attr.height model.canvasSize.height
                , Attr.width model.canvasSize.width
                ]
              <|
                Terrain.entities model.projectionMatrix viewMatrix model.terrain
            ]
