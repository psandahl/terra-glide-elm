module View exposing (view)

import Html exposing (Html)
import Html
import Html.Attributes as Attr
import Types exposing (Model, Msg)
import WebGL as GL


view : Model -> Html Msg
view model =
    Html.div
        []
        [ GL.toHtmlWith
            [ GL.antialias
            , GL.depth 1
            , GL.clearColor 0 0 0 0
            ]
            [ Attr.height model.canvasSize.height
            , Attr.width model.canvasSize.width
            ]
            []
        ]