module Main
    ( main
    ) where

import qualified PerlinProps
import qualified PerlinTests
import           Test.Framework                       (Test, defaultMain,
                                                       testGroup)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain testSuite

testSuite :: [Test]
testSuite =
    [ testGroup "Perlin algorithm proporties"
        [ testProperty "Must be in range [-1, 1]" PerlinProps.inRange
        , testProperty "Must be in range [0, 1]" PerlinProps.inNormalizedRange
        , testProperty "Shall be zero if integer input" PerlinProps.isZeroIfInteger
        ]
    , testGroup "Perlin raw file generation"
        [ testCase "Raw file data shall be the expected" PerlinTests.rawFileGeneration
        ]
    , testGroup "Mesh generation"
        [ testCase "Mesh dimensions shall be the expected"
                   PerlinTests.meshGenerationDimensions
        , testCase "Mesh contents shall be the expected"
                   PerlinTests.meshGenerationContents
        ]
    , testGroup "Index generation"
        [ testCase "Index generation 2x2" PerlinTests.indexGeneration2x2
        , testCase "Index generation 4x3" PerlinTests.indexGeneration4x3
        ]
    ]
