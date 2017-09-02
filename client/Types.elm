module Types exposing (Model, Msg(..), init)

import Window exposing (Size)


type alias Model =
    { canvasSize : Size
    }


type Msg
    = WindowSize Size


init : Model
init =
    { canvasSize = { width = 30, height = 30 } }
