module Types exposing (Model)

import Camera exposing (Camera)
import Math.Matrix4 exposing (Mat4)
import Terrain exposing (Terrain)
import Window exposing (Size)


{-| Application state.
-}
type alias Model =
    { canvasSize : Size
    , projectionMatrix : Mat4
    , camera : Camera
    , terrain : Terrain
    , errorMessage : Maybe String
    }
