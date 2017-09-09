module Update exposing (update)

import Debug
import Http exposing (Error(..))
import Model exposing (Model)
import Msg exposing (Msg(..))
import Projection
import Terrain


{-| The application's update function.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowSize size ->
            ( { model
                | canvasSize = size
                , projectionMatrix = Projection.makeProjection size
              }
            , Cmd.none
            )

        NewTileData startPosition result ->
            case result of
                Ok tileData ->
                    ( { model
                        | terrain = Terrain.addTile startPosition tileData model.terrain
                      }
                    , Cmd.none
                    )

                Err err ->
                    let
                        errMsg =
                            Debug.log "NewTileData: " <| errorToString err
                    in
                        ( { model | errorMessage = Just errMsg }, Cmd.none )


{-| Convert Http.Error to a string.
-}
errorToString : Error -> String
errorToString err =
    case err of
        BadUrl str ->
            "Bad Url: " ++ str

        Timeout ->
            "Time out. Request took too long time"

        NetworkError ->
            "Network Error"

        BadStatus resp ->
            "Bad response status: " ++ toString (resp.status)

        BadPayload str resp ->
            "Bad payload (JSON decode error): " ++ str
