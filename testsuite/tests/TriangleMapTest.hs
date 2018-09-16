module TriangleMapTest where

import Test.HUnit
import TriangleMap
import qualified Data.Map as Map

testData :: [(Integer, Integer, [((Integer, Integer), Integer)])]
testData = [
            (3, 6, [((0, 0), 0), ((0, 1), 1), ((0, 2), 2), ((1, 1), 3), ((1, 2), 4), ((2, 2), 5)]),
            (4, 10, [((0, 0), 0), ((0, 1), 1), ((0, 2), 2), ((0, 3), 3), ((1, 1), 4), ((1, 2), 5),
                     ((1, 3), 6), ((2, 2), 7), ((2, 3), 8), ((3, 3), 9)]),
            (10, 55, [((0, 0), 0), ((0, 1), 1), ((0, 2), 2), ((9, 9), 54)]),
            (100, 5050, []),
            (315, 49770, [((0, 0), 0), ((0, 108), 108), ((1, 1), 315), ((1, 6), 320), ((2, 173), 800),
                          ((314, 314), 49769)]),
            (799, 319600, [((0, 0), 0), ((1, 1), 799), ((798, 798), 319599)])
           ]

testTriangleMap :: Test
testTriangleMap = TestList $ testDimensions : testContents : []

testDimensions :: Test
testDimensions = TestList $ do (d, n, _) <- testData
                               let tMap = buildTriangleMap d
                               return $ TestCase $ assertEqual "" n (fromIntegral (Map.size tMap))

testContents :: Test
testContents = TestList $ do (d, _, l) <- testData
                             let tMap = buildTriangleMap d
                             ((i, j), a) <- l
                             let a' = tMap Map.! (i, j)
                             return $ TestCase $ assertEqual "" a a'
