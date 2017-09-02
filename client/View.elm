module View exposing (view)

import Html exposing (Html)
import Html
import Html.Attributes as Attr
import Types exposing (Model, Msg)
import WebGL as GL


view : Model -> Html Msg
view model =
    Html.div
        [ Attr.style
            [ ( "width", "100vw" )
            , ( "height", "100vh" )
            ]
        ]
        [ GL.toHtmlWith
            [ GL.antialias
            , GL.depth 1
            , GL.clearColor 0 0 0 0
            ]
            [ Attr.height 500
            , Attr.width 500
            ]
            []
        ]
