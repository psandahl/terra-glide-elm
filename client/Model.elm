module Model exposing (Model)

import Camera exposing (Camera)
import Environment exposing (Environment)
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
    , countDown : Int
    , projectionMatrix : Mat4
    , camera : Camera
    , environment : Environment
    , navigator : Navigator
    , skyDome : SkyDome
    , terrain : Terrain
    , water : Water
    , errorMessage : Maybe String
    }
