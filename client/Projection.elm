module Projection exposing (makeProjection, defaultWindowSize)

import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as Mat
import Window exposing (Size)


{-| Make a projection matrix given the
-}
makeProjection : Size -> Mat4
makeProjection windowSize =
    Mat.makePerspective 45 (toFloat windowSize.width / toFloat windowSize.height) 0.1 200


{-| Default window size to use before something is reported from Elm.
-}
defaultWindowSize : Size
defaultWindowSize =
    { width = 800, height = 600 }
