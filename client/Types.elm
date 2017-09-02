module Types exposing (Model, Msg(..), init)


type alias Model =
    { dummy : Int
    }


type Msg
    = NoOp


init : Model
init =
    { dummy = 1 }
