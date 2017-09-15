module Environment exposing (Environment, Gradient, init)

import Math.Vector3 exposing (Vec3, vec3, normalize)


{-| The Environment module presents values for colors, lights and stuff.
-}
type alias Environment =
    { sky : Gradient
    , lowerTerrain : Gradient
    , upperTerrain : Gradient
    , fogColor : Vec3
    , fogHeight : Float
    , waterColor : Vec3
    , ambientColor : Vec3
    , ambientStrength : Float
    , diffuseColor : Vec3
    , sunDirection : Vec3
    }


type alias Gradient =
    { lower : Vec3
    , upper : Vec3
    }


init : Environment
init =
    { sky =
        { lower = vec3 (170 / 255) (204 / 255) (204 / 255)
        , upper = vec3 (12 / 255) (94 / 255) (170 / 255)
        }
    , lowerTerrain =
        { lower = vec3 (239 / 255) (141 / 255) (55 / 255)
        , upper = vec3 0 1 0
        }
    , upperTerrain =
        { lower = vec3 (55 / 255) (68 / 255) (71 / 255)
        , upper = vec3 1 1 1
        }
    , fogColor = vec3 0.5 0.5 0.5
    , fogHeight = 0.2
    , waterColor = vec3 0 0 1
    , ambientColor = vec3 1 1 1
    , ambientStrength = 0.2
    , diffuseColor = vec3 0.8 0.8 0.8
    , sunDirection = normalize <| vec3 1 1 0
    }
