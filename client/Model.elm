module Model exposing (Model)

import Camera exposing (Camera)
import Math.Matrix4 exposing (Mat4)
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
    , terrain : Terrain
    , water : Water
    , errorMessage : Maybe String
    }
