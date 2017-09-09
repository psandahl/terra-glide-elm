module Constants exposing (tileSize, tileVista, terrainHeight)

{-| The length of the tile's side.
-}


tileSize : Int
tileSize =
    100


{-| The radius how far tiles must be available from the view position.
-}
tileVista : Float
tileVista =
    200


{-| The max height of the terrain.
-}
terrainHeight : Float
terrainHeight =
    100
