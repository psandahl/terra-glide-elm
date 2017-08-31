{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Perlin.Mesh
    ( Mesh (..)
    , Vertex (..)
    , generateMesh
    ) where

import           Data.Aeson
import           Data.Vector  (Vector, generate)
import           GHC.Generics (Generic)
import           Linear.V2    (V2 (..))
import           Linear.V3    (V3 (..))

data Mesh = Mesh
    { width    :: !Int
    , depth    :: !Int
    , vertices :: !(Vector Vertex)
    } deriving (Generic, Show, ToJSON)

data Vertex = Vertex
    { position :: !(V3 Float)
    , normal   :: !(V3 Float)
    , texCoord :: !(V2 Float)
    } deriving (Generic, Show, ToJSON)

instance ToJSON a => ToJSON (V2 a) where
    toJSON (V2 s t) = object["s" .= s, "t" .= t]

instance ToJSON a => ToJSON (V3 a) where
    toJSON (V3 x y z) = object["x" .= x, "y" .= y, "z" .= z]

generateMesh :: (Int -> Int -> V3 Float) -> Int -> Int -> Mesh
generateMesh g w d =
    let v = generate (w * d) mkVertex
    in Mesh { width = w
            , depth = d
            , vertices = v
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

-- | From a mesh width and a linear index, whatare the (x, z) coordinates.
fromIndex :: Int -> Int -> (Int, Int)
fromIndex w index =
    (index `mod` w, index `div` w)
{-# INLINE fromIndex #-}
