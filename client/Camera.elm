module Camera exposing (Camera, set)

import Math.Matrix4 as Mat
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 as Vec2
import Math.Vector2 exposing (Vec2)
import Math.Vector3 as Vec3
import Math.Vector3 exposing (Vec3)


{-| The camera record. The camera is dead simple, it is placed somewhere
in the world together with a view direction.
-}
type alias Camera =
    { position : Vec3
    , viewDirection : Vec3
    , viewMatrix : Mat4
    }


{-| Set the camera using position and view direction. The view direction is
not assumed to be normalized by the user as the set function will normalize it.
-}
set : Vec3 -> Vec2 -> Camera
set position viewDirection =
    let
        viewDirection3 =
            toNormalizedVec3 viewDirection
    in
        { position = position
        , viewDirection = viewDirection3
        , viewMatrix = makeMatrix position viewDirection3
        }


makeMatrix : Vec3 -> Vec3 -> Mat4
makeMatrix position viewDirection =
    let
        height =
            Vec3.getY position

        viewPointStraight =
            Vec3.add position <| Vec3.scale (height * 2) viewDirection

        viewPointGround =
            Vec3.setY 0 viewPointStraight
    in
        Mat.makeLookAt position viewPointGround <| Vec3.vec3 0 1 0


toNormalizedVec3 : Vec2 -> Vec3
toNormalizedVec3 vec =
    let
        vecNorm =
            Vec2.normalize vec
    in
        Vec3.vec3 (Vec2.getX vecNorm) 0 (Vec2.getY vecNorm)
