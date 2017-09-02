module Update exposing (update)

import Types exposing (Model, Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowSize size ->
            ( { model | canvasSize = size }, Cmd.none )

        LoadMeshData startX startZ result ->
            ( model, Cmd.none )
