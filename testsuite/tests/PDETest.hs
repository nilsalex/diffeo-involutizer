module PDETest where

import Test.HUnit
import PDE
import Control.Applicative
import Data.Ratio
import qualified Data.Map as Map

testDataRationals :: [Rational]
testDataRationals = [0%1, 2%1, 5%3, -9%4]

testDataRationals' :: [Rational]
testDataRationals' = [0%1, 3%1, -4%3, 5%2]

testDataIdeps = [0..3]

testDataDeps = [0..3]

testDataCoefficients :: [Coefficient]
testDataCoefficients = Null :
                        (map Constant testDataRationals) ++
                        (liftA2 Linear testDataRationals testDataIdeps)

testDataTerms :: [Term]
testDataTerms = liftA2 Term testDataCoefficients testDataDeps

testDataRandomIdepsMap :: Map.Map Integer Rational
testDataRandomIdepsMap = Map.fromList $ zip testDataIdeps testDataRationals'

testDataEvaluatedTerms :: [TermRand]
testDataEvaluatedTerms = map (\(c, d) -> TermRand c d)
                            [
                                (0, 0), (0, 1), (0, 2), (0, 3),
                                (0, 0), (0, 1), (0, 2), (0, 3),
                                (2, 0), (2, 1), (2, 2), (2, 3),
                                (5%3, 0), (5%3, 1), (5%3, 2), (5%3, 3),
                                (-9%4, 0), (-9%4, 1), (-9%4, 2), (-9%4, 3),
                                (0, 0), (0, 1), (0, 2), (0, 3),
                                (0, 0), (0, 1), (0, 2), (0, 3),
                                (0, 0), (0, 1), (0, 2), (0, 3),
                                (0, 0), (0, 1), (0, 2), (0, 3),
                                (0, 0), (0, 1), (0, 2), (0, 3),
                                (6, 0), (6, 1), (6, 2), (6, 3),
                                (-8%3, 0), (-8%3, 1), (-8%3, 2), (-8%3, 3),
                                (5, 0), (5, 1), (5, 2), (5, 3),
                                (0, 0), (0, 1), (0, 2), (0, 3),
                                (5, 0), (5, 1), (5, 2), (5, 3),
                                (-20%9, 0), (-20%9, 1), (-20%9, 2), (-20%9, 3),
                                (25%6, 0), (25%6, 1), (25%6, 2), (25%6, 3),
                                (0, 0), (0, 1), (0, 2), (0, 3),
                                (-27%4, 0), (-27%4, 1), (-27%4, 2), (-27%4, 3),
                                (3, 0), (3, 1), (3, 2), (3, 3),
                                (-45%8, 0), (-45%8, 1), (-45%8, 2), (-45%8, 3)
                            ]

testDataEvaluatedPDE :: PDERand
testDataEvaluatedPDE = map (\(c, d) -> TermRand c d)
                            [
                                (2, 0), (2, 1), (2, 2), (2, 3),
                                (5%3, 0), (5%3, 1), (5%3, 2), (5%3, 3),
                                (-9%4, 0), (-9%4, 1), (-9%4, 2), (-9%4, 3),
                                (6, 0), (6, 1), (6, 2), (6, 3),
                                (-8%3, 0), (-8%3, 1), (-8%3, 2), (-8%3, 3),
                                (5, 0), (5, 1), (5, 2), (5, 3),
                                (5, 0), (5, 1), (5, 2), (5, 3),
                                (-20%9, 0), (-20%9, 1), (-20%9, 2), (-20%9, 3),
                                (25%6, 0), (25%6, 1), (25%6, 2), (25%6, 3),
                                (-27%4, 0), (-27%4, 1), (-27%4, 2), (-27%4, 3),
                                (3, 0), (3, 1), (3, 2), (3, 3),
                                (-45%8, 0), (-45%8, 1), (-45%8, 2), (-45%8, 3)
                            ]

testDataPDENums :: [Integer]
testDataPDENums = [0, 2, 101]

testDataPDEMat :: [PDEMat]
testDataPDEMat =
                            [
                                [((0, 0), 65875%9000), ((0, 1), 65875%9000), ((0, 2), 65875%9000), ((0, 3), 65875%9000)],
                                [((2, 0), 65875%9000), ((2, 1), 65875%9000), ((2, 2), 65875%9000), ((2, 3), 65875%9000)],
                                [((101, 0), 65875%9000), ((101, 1), 65875%9000), ((101, 2), 65875%9000), ((101, 3), 65875%9000)]
                            ]

testDataPDESys :: [PDERand]
testDataPDESys = map (map (\(c, d) -> TermRand c d))
                            [
                                [(2%1, 4), (0%1, 3), (6%7, 11)],
                                [(5%9, 0), (1%2, 4), (6%1, 0)],
                                [(99%7, 8)]
                            ]

testDataPDESysMat :: PDEMat
testDataPDESysMat = [((0, 3), 0%1), ((0, 4), 2%1), ((0, 11), 6%7),
                     ((1, 0), 50%9), ((1, 4), 1%2),
                     ((3, 8), 99%7)]

testPDE :: Test
testPDE = TestList $ testEvalTermRand : testEvalPDERand : testRandPDEToMat : testRandPDESysToMat : []

testEvalTermRand :: Test
testEvalTermRand = TestList $ map TestCase $
                              zipWith
                                (assertEqual "")
                                (map (evalTermRand testDataRandomIdepsMap) testDataTerms)
                                testDataEvaluatedTerms

testEvalPDERand :: Test
testEvalPDERand = TestCase $ assertEqual "" (evalPDERand testDataRandomIdepsMap testDataTerms) testDataEvaluatedPDE

testRandPDEToMat :: Test
testRandPDEToMat = TestList $ zipWith (\mat i -> TestCase $ assertEqual "" mat (randPDEToMat i testDataEvaluatedPDE)) testDataPDEMat testDataPDENums

testRandPDESysToMat :: Test
testRandPDESysToMat = TestCase $ assertEqual "" testDataPDESysMat (randPDESysToMat testDataPDESys)
