module DiffeoEquationsArea where

import PDE
import PDEProlongation
import qualified IntertwinerArea
import qualified IntertwinerMetric
import qualified IntertwinerSym3
import Control.Applicative
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as S

spacetimeDimension :: Int
spacetimeDimension = 4

spacetimeSecondDimension = (spacetimeDimension * (spacetimeDimension + 1)) `div` 2

areaDimension :: Int
areaDimension = 21

spacetimeRange :: [Int]
spacetimeRange = [0..spacetimeDimension - 1]

spacetimeSecondRange :: [Int]
spacetimeSecondRange = [0..spacetimeSecondDimension - 1]

spacetimeThirdRange :: [Int]
spacetimeThirdRange = [0..((spacetimeDimension * (spacetimeDimension + 1) * (spacetimeDimension + 2)) `div` (1*2*3)) - 1]

areaRange :: [Int]
areaRange = [0..areaDimension - 1]

delta :: Num a => Int -> Int -> a
delta m n = if m == n then 1 else 0

areaFirstDerivative :: Int -> Int -> Int
areaFirstDerivative a i = areaDimension + a * spacetimeDimension + i

areaSecondDerivative :: Int -> Int -> Int
areaSecondDerivative a i = (spacetimeDimension + 1) * areaDimension + a * spacetimeSecondDimension + i

firstDimension :: Int
firstDimension = (1 + spacetimeDimension + spacetimeSecondDimension) * areaDimension

firstRange :: [Int]
firstRange = [0..firstDimension - 1]
{-
jetMap :: Int -> Int -> Int
jetMap derivative idep = firstDimension + (buildTriangleMap firstDimension) Map.! (min derivative idep, max derivative idep)

prolongAreaPDE :: PDE -> [PDE]
prolongAreaPDE pde = do derivative <- firstRange
                        return $ prolongation jetMap derivative pde

prolongAreaPDESystem :: [PDE] -> [PDE]
prolongAreaPDESystem = concat . (map prolongAreaPDE)
-}

areaPDE :: (Fractional a, Eq a) => PDESystem a
areaPDE = PDESys $ S.fromList $
           ((\m n -> let pde1 = areaPDE1Part1Inner areaIntertwinerMap m n
                         pde2 = areaPDE1Part2Inner areaIntertwinerMap m n
                         pde3 = areaPDE1Part3Inner areaIntertwinerMap metricIntertwinerMap m n
                     in addPDEs (addPDEs pde1 pde2) pde3)
             <$> spacetimeRange <*> spacetimeRange)
           ++
           ((\k n -> let pde1 = areaPDE2Part1Inner areaIntertwinerMap sym2IntertwinerMap k n
                         pde2 = areaPDE2Part2Inner areaIntertwinerMap metricIntertwinerMap k n
                         pde3 = areaPDE2Part3Inner k n
                     in addPDEs (addPDEs pde1 pde2) pde3)
             <$> spacetimeSecondRange <*> spacetimeRange)
           ++
           ((\k n -> areaPDE3Inner areaIntertwinerMap sym2ProjectorMap sym3IntertwinerMap k n)
             <$> spacetimeThirdRange <*> spacetimeRange)
           where
               areaIntertwinerMap = IntertwinerArea.buildAreaIntertwinerMap
               metricIntertwinerMap = IntertwinerMetric.buildMetricIntertwinerMap
               sym2IntertwinerMap = IntertwinerMetric.buildSym2IntertwinerMap
               sym2ProjectorMap = IntertwinerMetric.buildSym2ProjectorMap
               sym3IntertwinerMap = IntertwinerSym3.buildSym3IntertwinerMap
               sym3ProjectorMap = IntertwinerSym3.buildSym3ProjectorMap

areaPDE1Part1Inner :: (Fractional a, Eq a) => Map.Map (Int, Int, Int, Int) a -> Int -> Int -> PDE a
areaPDE1Part1Inner areaIntMap m n = foldl' addPDEs emptyPDE $
                                        (\a b -> let prefactor = areaIntMap Map.! (a, b, m, n)
                                                 in pdeFromConstIdepDerivative firstDimension prefactor b a)
                                        <$> areaRange <*> areaRange

areaPDE1Part2Inner :: (Fractional a, Eq a) => Map.Map (Int, Int, Int, Int) a -> Int -> Int -> PDE a
areaPDE1Part2Inner areaIntMap m n = foldl' addPDEs emptyPDE $
                                        (\a i b j -> let prefactor1 = delta i j * areaIntMap Map.! (a, b, m, n)
                                                         prefactor2 = -1 * delta a b * delta i m * delta j n
                                                         prefactor = prefactor1 + prefactor2
                                                         indicesAi = areaFirstDerivative a i
                                                         indicesBj = areaFirstDerivative b j
                                                     in pdeFromConstIdepDerivative firstDimension prefactor indicesBj indicesAi)
                                        <$> areaRange <*> spacetimeRange <*> areaRange <*> spacetimeRange

areaPDE1Part3Inner :: (Fractional a, Eq a) => Map.Map (Int, Int, Int, Int) a -> Map.Map (Int, Int, Int, Int) a -> Int -> Int -> PDE a
areaPDE1Part3Inner areaIntMap metricIntMap m n = foldl' addPDEs emptyPDE $
                                                    (\a i b j -> let prefactor1 = delta i j * areaIntMap Map.! (a, b, m, n)
                                                                     prefactor2 = -2 * delta a b * metricIntMap Map.! (i, j, m, n)
                                                                     prefactor = prefactor1 + prefactor2
                                                                     indicesAi = areaSecondDerivative a i
                                                                     indicesBj = areaSecondDerivative b j
                                                                 in pdeFromConstIdepDerivative firstDimension prefactor indicesBj indicesAi)
                                                    <$> areaRange <*> spacetimeSecondRange <*> areaRange <*> spacetimeSecondRange

areaPDE2Part1Inner :: (Fractional a, Eq a) => Map.Map (Int, Int, Int, Int) a -> Map.Map (Int, (Int, Int)) a -> Int -> Int -> PDE a
areaPDE2Part1Inner areaIntMap sym2IntMap k n = foldl' addPDEs emptyPDE $
                                                  (\a i b m -> let prefactor = areaIntMap Map.! (a, b, m, n) * sym2IntMap Map.! (k, (i, m))
                                                                   indicesAi = areaFirstDerivative a i
                                                               in pdeFromConstIdepDerivative firstDimension prefactor b indicesAi)
                                                  <$> areaRange <*> spacetimeRange <*> areaRange <*> spacetimeRange

areaPDE2Part2Inner :: (Fractional a, Eq a) => Map.Map (Int, Int, Int, Int) a -> Map.Map (Int, Int, Int, Int) a -> Int -> Int -> PDE a
areaPDE2Part2Inner areaIntMap metricIntMap k n = foldl' addPDEs emptyPDE $
                                                    (\a i b j m -> let prefactor = 2 * areaIntMap Map.! (a, b, m, n) * metricIntMap Map.! (i, k, j, m)
                                                                       indicesAi = areaSecondDerivative a i
                                                                       indicesBj = areaFirstDerivative b j
                                                                   in pdeFromConstIdepDerivative firstDimension prefactor indicesBj indicesAi)
                                                    <$> areaRange <*> spacetimeSecondRange <*> areaRange <*> spacetimeRange <*> spacetimeRange

areaPDE2Part3Inner :: (Fractional a, Eq a) => Int -> Int -> PDE a
areaPDE2Part3Inner k n = foldl' addPDEs emptyPDE $
                            (\a -> let prefactor = -1
                                       indicesAi = areaSecondDerivative a k
                                       indicesBj = areaFirstDerivative a n
                                   in pdeFromConstIdepDerivative firstDimension prefactor indicesBj indicesAi)
                            <$> areaRange

areaPDE3Inner :: (Fractional a, Eq a) => Map.Map (Int, Int, Int, Int) a -> Map.Map ((Int, Int), Int) a -> Map.Map (Int, (Int, Int, Int)) a -> Int -> Int -> PDE a
areaPDE3Inner areaIntMap sym2ProjMap sym3IntMap k n = foldl' addPDEs emptyPDE $
                                                         (\a i b m p q -> let prefactor = areaIntMap Map.! (a, b, m, n)
                                                                                          * sym2ProjMap Map.! ((p, q), i)
                                                                                          * sym3IntMap Map.! (k, (p, q, m))
                                                                              indicesAi = areaSecondDerivative a i
                                                                          in pdeFromConstIdepDerivative firstDimension prefactor b indicesAi)
                                                         <$> areaRange <*> spacetimeSecondRange <*> areaRange <*> spacetimeRange <*> spacetimeRange <*> spacetimeRange
