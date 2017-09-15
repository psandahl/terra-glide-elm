module Camera exposing (Camera, set)

import Math.Matrix4 as Mat
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 as Vec3
import Math.Vector3 exposing (Vec3)


{-| The camera record. The camera is dead simple, it is placed somewhere
in the world. Always pointing south (in position z), and with a pitch angle
in degrees. Zero degrees is looking straight ahead, a positive pitch is looking
down and a negative is looking up.
-}
type alias Camera =
    { position : Vec3
    , pitch : Float
    , viewMatrix : Mat4
    }


{-| Set the camera using position and pitch angle.
-}
set : Vec3 -> Float -> Camera
set position pitch =
    { position = position
    , pitch = pitch
    , viewMatrix = makeMatrix position pitch
    }


makeMatrix : Vec3 -> Float -> Mat4
makeMatrix position pitch =
    let
        pitchRotate =
            Mat.makeRotate (degrees pitch) <| Vec3.vec3 1 0 0

        viewVector =
            Mat.transform pitchRotate viewDirection

        lookAt =
            Vec3.add position viewVector
    in
        Mat.makeLookAt position lookAt <| Vec3.vec3 0 1 0


viewDirection : Vec3
viewDirection =
    Vec3.vec3 0 0 1
