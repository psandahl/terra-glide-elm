module Update exposing (update)

import Debug
import Types exposing (Model, Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowSize size ->
            ( { model | canvasSize = size }, Cmd.none )

        FetchTileData ( startX, startZ ) result ->
            let
                foo =
                    Debug.log "Result: " result
            in
                ( model, Cmd.none )
