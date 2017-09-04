module Camera exposing (Camera, init, viewMatrix)

import Math.Matrix4 as Mat
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, vec3, add)


type alias Camera =
    { position : Vec3
    , heading : Vec3
    }


init : Vec3 -> Vec3 -> Camera
init =
    Camera


viewMatrix : Camera -> Mat4
viewMatrix camera =
    Mat.makeLookAt camera.position (add camera.position camera.heading) (vec3 0 1 0)
