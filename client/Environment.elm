module Environment exposing (Environment, init)

import Math.Vector3 exposing (Vec3, vec3)


{-| The Environment module presents values for colors, lights and stuff.
-}
type alias Environment =
    { skyColor : Vec3
    , horizonColor : Vec3
    , fogColor : Vec3
    , fogHeight : Float
    , waterColor : Vec3
    }


init : Environment
init =
    { skyColor = vec3 (12 / 255) (94 / 255) (170 / 255)
    , horizonColor = vec3 (170 / 255) (204 / 255) (204 / 255)
    , fogColor = vec3 0.5 0.5 0.5
    , fogHeight = 0.2
    , waterColor = vec3 0 0 1
    }
