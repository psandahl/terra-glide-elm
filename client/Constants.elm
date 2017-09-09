module Constants exposing (tileSize, tileVista, terrainHeight, cameraHeight)

{-| The length of the tile's side.
-}


tileSize : Int
tileSize =
    50


{-| The radius how far tiles must be available from the view position.
-}
tileVista : Float
tileVista =
    400


{-| The max height of the terrain.
-}
terrainHeight : Float
terrainHeight =
    300


{-| The camera's height. Always 50 units above of the heighest terrain.
-}
cameraHeight : Float
cameraHeight =
    terrainHeight + 50
