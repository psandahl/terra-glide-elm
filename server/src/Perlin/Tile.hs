{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Perlin.Tile
    ( TileData (..)
    , Vertex (..)
    , generateTileData
    , generateIndices
    ) where

import           Control.Monad           (forM_)
import           Control.Monad.Primitive (PrimMonad)
import           Control.Monad.ST        (runST)
import           Data.Aeson
import           Data.Vector             (Vector, (!?))
import qualified Data.Vector             as Vector
import qualified Data.Vector.Mutable     as MVector
import           GHC.Generics            (Generic)
import           Linear.Metric           (normalize)
import           Linear.V2               (V2 (..))
import           Linear.V3               (V3 (..), cross)

-- | Tile data definition. All vertices in the mesh will be in world space.
data TileData = TileData
    { width    :: !Int
    , depth    :: !Int
    , vertices :: !(Vector Vertex)
    , indices  :: !(Vector Int)
    } deriving (Generic, Show, ToJSON)

-- | Vertex definition.
data Vertex = Vertex
    { position :: !(V3 Float)
    , normal   :: !(V3 Float)
    , texCoord :: !(V2 Float)
    } deriving (Generic, Show, ToJSON)

instance ToJSON a => ToJSON (V2 a) where
    toJSON (V2 s t) = object["s" .= s, "t" .= t]

instance ToJSON a => ToJSON (V3 a) where
    toJSON (V3 x y z) = object["x" .= x, "y" .= y, "z" .= z]

-- | Generate TileData with a Mesh of size w * d vertices.
generateTileData :: (Int -> Int -> V3 Float) -> Int -> Int -> TileData
generateTileData g w d =
    let indices' = generateIndices w d
    in TileData { width = w
                , depth = d
                , vertices = smoothNormals indices' $
                                Vector.generate (w * d) mkVertex
                , indices = indices'
                }
    where
        mkVertex :: Int -> Vertex
        mkVertex index =
            let (x, z) = fromIndex w index
            in Vertex
                 { position = g x z
                 , normal = V3 0 0 0
                 , texCoord = V2 (fromIntegral x) (fromIntegral (d - 1 - z))
                 }

-- | From a mesh width and a linear index, find its (x, z) coordinates.
fromIndex :: Int -> Int -> (Int, Int)
fromIndex w index =
    (index `mod` w, index `div` w)
{-# INLINE fromIndex #-}

-- | Generate indices for a grid of width x height vertices.
generateIndices :: Int -> Int -> Vector Int
generateIndices w d =
    Vector.concatMap (\row ->
        Vector.concatMap (\col ->
            let upperLeft = row * w + col
                upperRight = upperLeft + 1
                lowerLeft = (row + 1) * w + col
                lowerRight = lowerLeft + 1
            in Vector.fromList [upperRight, upperLeft, lowerLeft, upperRight, lowerLeft, lowerRight]
        ) (Vector.fromList [0 .. w - 2])
    ) (Vector.fromList [0 .. d - 2])

-- | Smooth all vertices.
smoothNormals :: Vector Int -> Vector Vertex -> Vector Vertex
smoothNormals indices' inputVertices =
    runST $ do
        mutableVertices <- Vector.unsafeThaw inputVertices

        -- Per surface, calculate its normal and add that surface normal
        -- to each vertice.
        perSurface
            (\(i1, i2, i3) -> do
                v1 <- MVector.read mutableVertices i1
                v2 <- MVector.read mutableVertices i2
                v3 <- MVector.read mutableVertices i3

                let sn = surfaceNormal (position v1)
                                       (position v2)
                                       (position v3)

                MVector.write mutableVertices i1 $ v1 { normal = normal v1 + sn }
                MVector.write mutableVertices i2 $ v2 { normal = normal v2 + sn }
                MVector.write mutableVertices i3 $ v3 { normal = normal v3 + sn }
            ) indices'

        -- Traverse all vertices and normalize their normals.
        forM_ [0 .. MVector.length mutableVertices - 1] $
            MVector.modify mutableVertices (\v -> v { normal = normalize (normal v)})

        Vector.unsafeFreeze mutableVertices

perSurface :: PrimMonad m => ((Int, Int, Int) -> m ()) -> Vector Int -> m ()
perSurface g indexVector = go 0
    where
        go baseIndex =
            case (,,) <$> indexVector !? baseIndex
                      <*> indexVector !? (baseIndex + 1)
                      <*> indexVector !? (baseIndex + 2) of
                Just indices'  -> do
                    g indices'
                    go (baseIndex + 3)

                Nothing -> return ()

surfaceNormal :: V3 Float -> V3 Float -> V3 Float -> V3 Float
surfaceNormal p1 p2 p3 =
    let v1 = p2 - p1
        v2 = p3 - p1
    in normalize $ v1 `cross` v2
{-# INLINE surfaceNormal #-}
