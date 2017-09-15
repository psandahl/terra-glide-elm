module Model exposing (Model)

import Camera exposing (Camera)
import Math.Matrix4 exposing (Mat4)
import Navigator exposing (Navigator)
import SkyDome exposing (SkyDome)
import Terrain exposing (Terrain)
import Water exposing (Water)
import Window exposing (Size)


{-| Application state.
-}
type alias Model =
    { canvasSize : Size
    , projectionMatrix : Mat4
    , camera : Camera
    , cameraRotation : Float
    , navigator : Navigator
    , skyDome : SkyDome
    , terrain : Terrain
    , water : Water
    , errorMessage : Maybe String
    }
