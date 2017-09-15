module Geometry
    exposing
        ( tileSize
        , tileVista
        , terrainHeight
        , waterHeight
        , cameraHeight
        , maxWorld
        )

{-| The Geometry module presents constants for definition of geometry.
-}


{-| The length of the tile's side.
-}
tileSize : Int
tileSize =
    50


{-| The radius how far tiles must be available from the view position.
-}
tileVista : Float
tileVista =
    300


{-| The max height of the terrain.
-}
terrainHeight : Float
terrainHeight =
    200


{-| The height of the water.
-}
waterHeight : Float
waterHeight =
    75


{-| The camera's height. Always 50 units above of the heighest terrain.
-}
cameraHeight : Float
cameraHeight =
    terrainHeight + 1


{-| The max size in x and z dimensions for the world.
-}
maxWorld : Float
maxWorld =
    1000000000