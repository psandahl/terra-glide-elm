module Update exposing (update)

import Camera
import Debug
import Http
import Math.Vector2 exposing (vec2)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Projection
import Terrain
import WebGL.Texture as Texture


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
                            Debug.log "NewTileData: " <| httpErrorToString err
                    in
                        ( { model | errorMessage = Just errMsg }, Cmd.none )

        Animate diff ->
            let
                newRotation =
                    (diff * 0.1) + model.cameraRotation

                viewVector =
                    vec2 (sin newRotation) (cos newRotation)
            in
                ( { model
                    | camera = Camera.set model.camera.position viewVector
                    , cameraRotation = newRotation
                  }
                , Cmd.none
                )

        DirtTexture result ->
            case result of
                Ok dirt ->
                    ( { model
                        | terrain = Terrain.addDirt dirt model.terrain
                      }
                    , Cmd.none
                    )

                Err err ->
                    let
                        errMsg =
                            Debug.log "DirtTexture: " <| textureErrorToString err
                    in
                        ( { model | errorMessage = Just errMsg }, Cmd.none )

        GrassTexture result ->
            case result of
                Ok grass ->
                    ( { model
                        | terrain = Terrain.addGrass grass model.terrain
                      }
                    , Cmd.none
                    )

                Err err ->
                    let
                        errMsg =
                            Debug.log "GrassTexture: " <| textureErrorToString err
                    in
                        ( { model | errorMessage = Just errMsg }, Cmd.none )


{-| Convert Http.Error to a string.
-}
httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl str ->
            "Bad Url: " ++ str

        Http.Timeout ->
            "Time out. Request took too long time"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus resp ->
            "Bad response status: " ++ toString (resp.status)

        Http.BadPayload str resp ->
            "Bad payload (JSON decode error): " ++ str


{-| Convert Texture.Error to a string.
-}
textureErrorToString : Texture.Error -> String
textureErrorToString err =
    case err of
        Texture.LoadError ->
            "Texture Load Error"

        Texture.SizeError _ _ ->
            "Texture Size Error (not power of 2)"
