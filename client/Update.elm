module Update exposing (update)

import Camera
import Debug
import Geometry
import Http
import Math.Vector2 as Vec2
import Math.Vector3 exposing (vec3)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Navigator
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
                            Debug.log "NewTileData: " <| httpErrorToString err
                    in
                        ( { model | errorMessage = Just errMsg }, Cmd.none )

        Animate time ->
            let
                newNavigator =
                    Navigator.animate time model.navigator

                newCameraPosition =
                    vec3 (Vec2.getX newNavigator.position)
                        Geometry.cameraHeight
                        (Vec2.getY newNavigator.position)

                newCamera =
                    Camera.set newCameraPosition 0

                newTerrain =
                    Terrain.purgePassedTiles newNavigator.position model.terrain
            in
                ( { model
                    | navigator = newNavigator
                    , camera = newCamera
                    , terrain = newTerrain
                  }
                , Navigator.runTileQueries newTerrain newNavigator
                )


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
