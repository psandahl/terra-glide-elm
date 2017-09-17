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
    , fogDistance : Float
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
        { lower = vec3 (246 / 255) (176 / 255) (133 / 255)
        , upper = vec3 (70 / 255) (106 / 255) (200 / 255)
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
    , fogDistance = 1200
    , waterColor = vec3 0 0 1
    , ambientColor = vec3 1 1 1
    , ambientStrength = 0.2
    , diffuseColor = vec3 (182 / 255) (126 / 255) (91 / 255)
    , sunDirection = normalize <| vec3 2 1 0
    }
