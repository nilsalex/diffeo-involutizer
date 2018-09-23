module DiffeoEquationsArea where

import PDE
import qualified IntertwinerArea
import qualified IntertwinerMetric
import qualified IntertwinerSym3
import Data.List (foldl')
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as S

spacetimeDimension :: Int
spacetimeDimension = 4

spacetimeSecondDimension :: Int
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
areaPDE = PDESys firstDimension $ S.fromList $
           ((\m n -> let pde1 = areaPDE1Part1Inner aIMap m n
                         pde2 = areaPDE1Part2Inner aIMap m n
                         pde3 = areaPDE1Part3Inner aIMap mIMap m n
                           in addPDEs (addPDEs pde1 pde2) pde3)
             <$> spacetimeRange <*> spacetimeRange)
           ++
           ((\k n -> let pde1 = areaPDE2Part1Inner aIMap s2IMap k n
                         pde2 = areaPDE2Part2Inner aIMap mIMap k n
                         pde3 = areaPDE2Part3Inner k n
                     in addPDEs (addPDEs pde1 pde2) pde3)
             <$> spacetimeSecondRange <*> spacetimeRange)
           ++
           ((\k n -> areaPDE3Inner aIMap s2PMap s3IMap k n)
             <$> spacetimeThirdRange <*> spacetimeRange)
           where
               aIMap = IntertwinerArea.buildAreaIntertwinerMap
               mIMap = IntertwinerMetric.buildMetricIntertwinerMap
               s2IMap = IntertwinerMetric.buildSym2IntertwinerMap
               s2PMap = IntertwinerMetric.buildSym2ProjectorMap
               s3IMap = IntertwinerSym3.buildSym3IntertwinerMap

areaPDE1Part1Inner :: (Fractional a, Eq a) => Map.Map (Int, Int, Int, Int) a -> Int -> Int -> PDE a
areaPDE1Part1Inner areaIntMap m n = foldl' addPDEs (emptyPDE firstDimension) $
                                        catMaybes $
                                        (\a b -> let prefactor = areaIntMap Map.! (a, b, m, n)
                                                 in if prefactor == 0 then Nothing
                                                                      else Just $ pdeFromConstIdepDerivative firstDimension prefactor b a)
                                        <$> areaRange <*> areaRange

areaPDE1Part2Inner :: (Fractional a, Eq a) => Map.Map (Int, Int, Int, Int) a -> Int -> Int -> PDE a
areaPDE1Part2Inner areaIntMap m n = foldl' addPDEs (emptyPDE firstDimension) $
                                        catMaybes $
                                        (\a i b j -> let prefactor1 = delta i j * areaIntMap Map.! (a, b, m, n)
                                                         prefactor2 = -1 * delta a b * delta i m * delta j n
                                                         prefactor = prefactor1 + prefactor2
                                                         indicesAi = areaFirstDerivative a i
                                                         indicesBj = areaFirstDerivative b j
                                                     in if prefactor == 0 then Nothing
                                                                          else Just $ pdeFromConstIdepDerivative
                                                                                      firstDimension prefactor indicesBj indicesAi)
                                        <$> areaRange <*> spacetimeRange <*> areaRange <*> spacetimeRange

areaPDE1Part3Inner :: (Fractional a, Eq a) => Map.Map (Int, Int, Int, Int) a -> Map.Map (Int, Int, Int, Int) a -> Int -> Int -> PDE a
areaPDE1Part3Inner areaIntMap metricIntMap m n = foldl' addPDEs (emptyPDE firstDimension) $
                                                 catMaybes $
                                                    (\a i b j -> let prefactor1 = delta i j * areaIntMap Map.! (a, b, m, n)
                                                                     prefactor2 = -2 * delta a b * metricIntMap Map.! (i, j, m, n)
                                                                     prefactor = prefactor1 + prefactor2
                                                                     indicesAi = areaSecondDerivative a i
                                                                     indicesBj = areaSecondDerivative b j
                                                                 in if prefactor == 0 then Nothing
                                                                                      else Just $ pdeFromConstIdepDerivative
                                                                                                  firstDimension prefactor indicesBj indicesAi)
                                                    <$> areaRange <*> spacetimeSecondRange <*> areaRange <*> spacetimeSecondRange

areaPDE2Part1Inner :: (Fractional a, Eq a) => Map.Map (Int, Int, Int, Int) a -> Map.Map (Int, (Int, Int)) a -> Int -> Int -> PDE a
areaPDE2Part1Inner areaIntMap sym2IntMap k n = foldl' addPDEs (emptyPDE firstDimension) $
                                               catMaybes $
                                                  (\a i b m -> let prefactor = areaIntMap Map.! (a, b, m, n) * sym2IntMap Map.! (k, (i, m))
                                                                   indicesAi = areaFirstDerivative a i
                                                               in if prefactor == 0 then Nothing
                                                                                    else Just $ pdeFromConstIdepDerivative
                                                                                                firstDimension prefactor b indicesAi)
                                                  <$> areaRange <*> spacetimeRange <*> areaRange <*> spacetimeRange

areaPDE2Part2Inner :: (Fractional a, Eq a) => Map.Map (Int, Int, Int, Int) a -> Map.Map (Int, Int, Int, Int) a -> Int -> Int -> PDE a
areaPDE2Part2Inner areaIntMap metricIntMap k n = foldl' addPDEs (emptyPDE firstDimension) $
                                                 catMaybes $
                                                    (\a i b j m -> let prefactor = 2 * areaIntMap Map.! (a, b, m, n) * metricIntMap Map.! (i, k, j, m)
                                                                       indicesAi = areaSecondDerivative a i
                                                                       indicesBj = areaFirstDerivative b j
                                                                   in if prefactor == 0 then Nothing
                                                                                        else Just $ pdeFromConstIdepDerivative
                                                                                                    firstDimension prefactor indicesBj indicesAi)
                                                    <$> areaRange <*> spacetimeSecondRange <*> areaRange <*> spacetimeRange <*> spacetimeRange

areaPDE2Part3Inner :: (Fractional a, Eq a) => Int -> Int -> PDE a
areaPDE2Part3Inner k n = foldl' addPDEs (emptyPDE firstDimension) $
                         catMaybes $
                            (\a -> let prefactor = -1
                                       indicesAi = areaSecondDerivative a k
                                       indicesBj = areaFirstDerivative a n
                                   in if prefactor == 0 then Nothing
                                                        else Just $ pdeFromConstIdepDerivative
                                                                    firstDimension prefactor indicesBj indicesAi)
                            <$> areaRange

areaPDE3Inner :: (Fractional a, Eq a) => Map.Map (Int, Int, Int, Int) a -> Map.Map ((Int, Int), Int) a -> Map.Map (Int, (Int, Int, Int)) a -> Int -> Int -> PDE a
areaPDE3Inner areaIntMap sym2ProjMap sym3IntMap k n = foldl' addPDEs (emptyPDE firstDimension) $
                                                      catMaybes $
                                                         (\a i b m p q -> let prefactor = areaIntMap Map.! (a, b, m, n)
                                                                                          * sym2ProjMap Map.! ((p, q), i)
                                                                                          * sym3IntMap Map.! (k, (p, q, m))
                                                                              indicesAi = areaSecondDerivative a i
                                                                          in if prefactor == 0 then Nothing
                                                                                               else Just $ pdeFromConstIdepDerivative
                                                                                                           firstDimension prefactor b indicesAi)
                                                         <$> areaRange <*>
                                                             spacetimeSecondRange <*>
                                                             areaRange <*>
                                                             spacetimeRange <*>
                                                             spacetimeRange <*>
                                                             spacetimeRange
