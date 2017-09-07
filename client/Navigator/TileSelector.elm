module Navigator.TileSelector exposing (farLeft)

import Constants exposing (tileVista)


farLeft : ( Float, Float ) -> ( Float, Float )
farLeft ( x, z ) =
    ( x - tileVista, z - tileVista )
