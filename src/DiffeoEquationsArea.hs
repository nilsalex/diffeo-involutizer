module DiffeoEquationsArea where

import PDE
import PDEProlongation
import TriangleMap
import qualified IntertwinerArea
import qualified IntertwinerMetric
import qualified IntertwinerSym3
import qualified Data.Map.Strict as Map
import Data.Ratio

spacetimeDimension :: Integer
spacetimeDimension = 4

spacetimeSecondDimension = (spacetimeDimension * (spacetimeDimension + 1)) `div` 2

areaDimension :: Integer
areaDimension = 21

spacetimeRange :: [Integer]
spacetimeRange = [0..spacetimeDimension - 1]

spacetimeSecondRange :: [Integer]
spacetimeSecondRange = [0..spacetimeSecondDimension - 1]

spacetimeThirdRange :: [Integer]
spacetimeThirdRange = [0..((spacetimeDimension * (spacetimeDimension + 1) * (spacetimeDimension + 2)) `div` (1*2*3)) - 1]

areaRange :: [Integer]
areaRange = [0..areaDimension - 1]

delta :: Integer -> Integer -> Rational
delta m n = if m == n then 1 % 1 else 0 % 1

areaFirstDerivative :: Integer -> Integer -> Integer
areaFirstDerivative a i = areaDimension + a * spacetimeDimension + i

areaSecondDerivative :: Integer -> Integer -> Integer
areaSecondDerivative a i = (spacetimeDimension + 1) * areaDimension + a * spacetimeSecondDimension + i

firstDimension :: Integer
firstDimension = (1 + spacetimeDimension + spacetimeSecondDimension) * areaDimension

firstRange :: [Integer]
firstRange = [0..firstDimension - 1]

jetMap :: Integer -> Integer -> Integer
jetMap derivative idep = firstDimension + (buildTriangleMap firstDimension) Map.! (min derivative idep, max derivative idep)

prolongAreaPDE :: PDE -> [PDE]
prolongAreaPDE pde = do derivative <- firstRange
                        return $ prolongation jetMap derivative pde

prolongAreaPDESystem :: [PDE] -> [PDE]
prolongAreaPDESystem = concat . (map prolongAreaPDE)

areaPDE1 :: [PDE]
areaPDE1 = a ++ b ++ c
           where
               areaIntertwinerMap = IntertwinerArea.buildAreaIntertwinerMap
               metricIntertwinerMap = IntertwinerMetric.buildMetricIntertwinerMap
               sym2IntertwinerMap = IntertwinerMetric.buildSym2IntertwinerMap
               sym2ProjectorMap = IntertwinerMetric.buildSym2ProjectorMap
               sym3IntertwinerMap = IntertwinerSym3.buildSym3IntertwinerMap
               sym3ProjectorMap = IntertwinerSym3.buildSym3ProjectorMap
               a = do m <- spacetimeRange
                      n <- spacetimeRange
                      let pde1 = areaPDE1Part1Inner areaIntertwinerMap m n
                      let pde2 = areaPDE1Part2Inner areaIntertwinerMap m n
                      let pde3 = areaPDE1Part3Inner areaIntertwinerMap metricIntertwinerMap m n
                      let pde1Reduced = filter (\Term { coefficient = c } -> c /= Null) pde1
                      let pde2Reduced = filter (\Term { coefficient = c } -> c /= Null) pde2
                      let pde3Reduced = filter (\Term { coefficient = c } -> c /= Null) pde3
                      return $ pde1Reduced ++ pde2Reduced ++ pde3Reduced
               b = do k <- spacetimeSecondRange
                      n <- spacetimeRange
                      let pde1 = areaPDE2Part1Inner areaIntertwinerMap sym2IntertwinerMap k n
                      let pde2 = areaPDE2Part2Inner areaIntertwinerMap metricIntertwinerMap k n
                      let pde3 = areaPDE2Part3Inner k n
                      let pde1Reduced = filter (\Term { coefficient = c } -> c /= Null) pde1
                      let pde2Reduced = filter (\Term { coefficient = c } -> c /= Null) pde2
                      let pde3Reduced = filter (\Term { coefficient = c } -> c /= Null) pde3
                      return $ pde1Reduced ++ pde2Reduced ++ pde3Reduced
               c = do k <- spacetimeThirdRange
                      n <- spacetimeRange
                      let pde = areaPDE3Inner areaIntertwinerMap sym2ProjectorMap sym3IntertwinerMap k n
                      let pdeReduced = filter (\Term { coefficient = c } -> c /= Null) pde
                      return pdeReduced

areaPDE1Part1Inner :: Map.Map (Integer, Integer, Integer, Integer) Rational -> Integer -> Integer -> PDE
areaPDE1Part1Inner areaIntMap m n = do a <- areaRange
                                       b <- areaRange
                                       let prefactor = areaIntMap Map.! (a, b, m, n)
                                       return $ Term { coefficient = if prefactor == 0 then Null else Linear prefactor b
                                                       , dependent = a }

areaPDE1Part2Inner :: Map.Map (Integer, Integer, Integer, Integer) Rational -> Integer -> Integer -> PDE
areaPDE1Part2Inner areaIntMap m n = concat $ 
                                    do a <- areaRange
                                       i <- spacetimeRange
                                       b <- areaRange
                                       j <- spacetimeRange
                                       let prefactor1 = delta i j * areaIntMap Map.! (a, b, m, n)
                                       let prefactor2 = -1 * delta a b * delta i m * delta j n
                                       let indicesAi = areaFirstDerivative a i
                                       let indicesBj = areaFirstDerivative b j
                                       return $ [ Term { coefficient = if prefactor1 == 0 then Null else Linear prefactor1 indicesBj
                                                         , dependent = indicesAi },
                                                  Term { coefficient = if prefactor2 == 0 then Null else Linear prefactor2 indicesBj
                                                         , dependent = indicesAi } ]

areaPDE1Part3Inner :: Map.Map (Integer, Integer, Integer, Integer) Rational -> Map.Map (Integer, Integer, Integer, Integer) Rational -> Integer -> Integer -> PDE
areaPDE1Part3Inner areaIntMap metricIntMap m n = concat $ 
                                                 do a <- areaRange
                                                    i <- spacetimeSecondRange
                                                    b <- areaRange
                                                    j <- spacetimeSecondRange
                                                    let prefactor1 = delta i j * areaIntMap Map.! (a, b, m, n)
                                                    let prefactor2 = -2 * delta a b * metricIntMap Map.! (i, j, m, n)
                                                    let indicesAi = areaSecondDerivative a i
                                                    let indicesBj = areaSecondDerivative b j
                                                    return $ [ Term { coefficient = if prefactor1 == 0 then Null else Linear prefactor1 indicesBj
                                                                      , dependent = indicesAi },
                                                               Term { coefficient = if prefactor2 == 0 then Null else Linear prefactor2 indicesBj
                                                                      , dependent = indicesAi } ]

areaPDE2Part1Inner :: Map.Map (Integer, Integer, Integer, Integer) Rational -> Map.Map (Integer, (Integer, Integer)) Rational -> Integer -> Integer -> PDE
areaPDE2Part1Inner areaIntMap sym2IntMap k n = do a <- areaRange
                                                  i <- spacetimeRange
                                                  b <- areaRange
                                                  m <- spacetimeRange
                                                  let prefactor = areaIntMap Map.! (a, b, m, n) * sym2IntMap Map.! (k, (i, m))
                                                  let indicesAi = areaFirstDerivative a i
                                                  return $ Term { coefficient = if prefactor == 0 then Null else Linear prefactor b
                                                                , dependent = indicesAi }

areaPDE2Part2Inner :: Map.Map (Integer, Integer, Integer, Integer) Rational -> Map.Map (Integer, Integer, Integer, Integer) Rational -> Integer -> Integer -> PDE
areaPDE2Part2Inner areaIntMap metricIntMap k n = do a <- areaRange
                                                    i <- spacetimeSecondRange
                                                    b <- areaRange
                                                    j <- spacetimeRange
                                                    m <- spacetimeRange
                                                    let prefactor = 2 * areaIntMap Map.! (a, b, m, n) * metricIntMap Map.! (i, k, j, m)
                                                    let indicesAi = areaSecondDerivative a i
                                                    let indicesBj = areaFirstDerivative b j
                                                    return $ Term { coefficient = if prefactor == 0 then Null else Linear prefactor indicesBj
                                                                  , dependent = indicesAi }

areaPDE2Part3Inner :: Integer -> Integer -> PDE
areaPDE2Part3Inner k n = do a <- areaRange
                            let b = a
                            let i = k
                            let j = n
                            let prefactor = -1
                            let indicesAi = areaSecondDerivative a i
                            let indicesBj = areaFirstDerivative b j
                            return $ Term { coefficient = Linear prefactor indicesBj
                                          , dependent = indicesAi }

areaPDE3Inner :: Map.Map (Integer, Integer, Integer, Integer) Rational -> Map.Map ((Integer, Integer), Integer) Rational -> Map.Map (Integer, (Integer, Integer, Integer)) Rational -> Integer -> Integer -> PDE
areaPDE3Inner areaIntMap sym2ProjMap sym3IntMap k n = do a <- areaRange
                                                         i <- spacetimeSecondRange
                                                         b <- areaRange
                                                         m <- spacetimeRange
                                                         p <- spacetimeRange
                                                         q <- spacetimeRange
                                                         let prefactor = areaIntMap Map.! (a, b, m, n) * sym2ProjMap Map.! ((p, q), i) * sym3IntMap Map.! (k, (p, q, m))
                                                         let indicesAi = areaSecondDerivative a i
                                                         return $ Term { coefficient = if prefactor == 0 then Null else Linear prefactor b
                                                                       , dependent = indicesAi }
