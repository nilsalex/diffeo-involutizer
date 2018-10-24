module DiffeoEquationsMetric (metricPDE, evalMetricAtEta) where

import PDE
import qualified IntertwinerMetric
import qualified IntertwinerSym3
import Index
import Control.Parallel.Strategies
import Data.List (foldl')
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as S

spacetimeDimension :: Int
spacetimeDimension = 4

spacetimeSecondDimension :: Int
spacetimeSecondDimension = (spacetimeDimension * (spacetimeDimension + 1)) `div` 2

areaDimension :: Int
areaDimension = 10

spacetimeRange :: [Int]
spacetimeRange = [0..spacetimeDimension - 1]

spacetimeRangeUp :: [ISUp]
spacetimeRangeUp = map ISUp spacetimeRange

spacetimeRangeDown :: [ISDown]
spacetimeRangeDown = map ISDown spacetimeRange

spacetimeSecondRange :: [Int]
spacetimeSecondRange = [0..spacetimeSecondDimension - 1]

spacetimeSecondRangeUp :: [IS2Up]
spacetimeSecondRangeUp = map IS2Up spacetimeSecondRange

spacetimeSecondRangeDown :: [IS2Down]
spacetimeSecondRangeDown = map IS2Down spacetimeSecondRange

spacetimeThirdRange :: [Int]
spacetimeThirdRange = [0..((spacetimeDimension * (spacetimeDimension + 1) * (spacetimeDimension + 2)) `div` (1*2*3)) - 1]

spacetimeThirdRangeUp :: [IS3Up]
spacetimeThirdRangeUp = map IS3Up spacetimeThirdRange

spacetimeThirdRangeDown :: [IS3Down]
spacetimeThirdRangeDown = map IS3Down spacetimeThirdRange

areaRange :: [Int]
areaRange = [0..areaDimension - 1]

areaRangeUp :: [IGUp]
areaRangeUp = map IGUp areaRange

areaRangeDown :: [IGDown]
areaRangeDown = map IGDown areaRange

deltaG :: Num a => IGUp -> IGDown -> a
deltaG (IGUp m) (IGDown n) = if m == n then 1 else 0

deltaS :: Num a => ISUp -> ISDown -> a
deltaS (ISUp m) (ISDown n) = if m == n then 1 else 0

deltaS2 :: Num a => IS2Up -> IS2Down -> a
deltaS2 (IS2Up m) (IS2Down n) = if m == n then 1 else 0

areaFirstDerivative :: Int -> Int -> Int
areaFirstDerivative a i = areaDimension + a * spacetimeDimension + i

areaSecondDerivative :: Int -> Int -> Int
areaSecondDerivative a i = (spacetimeDimension + 1) * areaDimension + a * spacetimeSecondDimension + i

firstDimension :: Int
firstDimension = (1 + spacetimeDimension + spacetimeSecondDimension) * areaDimension

firstRange :: [Int]
firstRange = [0..firstDimension - 1]

buildEtaMap :: (Num a, Eq a) => Map.Map Int a
buildEtaMap = Map.fromList $ zip firstRange $ [1, 0, 0, 0, -1, 0, 0, -1, 0, -1] ++ repeat 0

evalMetricAtEta :: (Num a, Eq a, Fractional a) => PDESystem a -> PDESystem a
evalMetricAtEta = evalPDESystem buildEtaMap

metricPDE :: (Fractional a, Eq a) => PDESystem a
metricPDE = runEval $
              do
                pde1' <- parTraversable rpar pde1
                pde2' <- parTraversable rpar pde2
                pde3' <- parTraversable rpar pde3
                return $ PDESys firstDimension $
                                 pde1' S.><
                                 pde2' S.><
                                 pde3'
               where
                   aIMap = IntertwinerMetric.buildMetricIntertwinerMap
                   mIMap = aIMap
                   s2IMap = IntertwinerMetric.buildSym2IntertwinerMap
                   s2PMap = IntertwinerMetric.buildSym2ProjectorMap
                   s3IMap = IntertwinerSym3.buildSym3IntertwinerMap
                   pde1 = areaPDE1 aIMap mIMap
                   pde2 = areaPDE2 aIMap mIMap s2IMap
                   pde3 = areaPDE3 aIMap s2PMap s3IMap

areaPDE1 :: (Fractional a, Eq a) => Map.Map (IGDown, IGUp, ISUp, ISDown) a ->
                                    Map.Map (IGDown, IGUp, ISUp, ISDown) a ->
                                    S.Seq (PDE a)
areaPDE1 aIMap mIMap = (\m n -> let pde1 = areaPDE1Part1Inner aIMap m n
                                    pde2 = areaPDE1Part2Inner aIMap m n
                                    pde3 = areaPDE1Part3Inner aIMap mIMap m n
                                in if (deltaS m n == (1 :: Int)) then addPDEs (addPDEs (addPDEs pde1 pde2) pde3)
                                                                              (pdeFromConstDep firstDimension 1)
                                                                 else addPDEs (addPDEs pde1 pde2) pde3)
                       <$> (S.fromList spacetimeRangeUp) <*> (S.fromList spacetimeRangeDown)

areaPDE2 :: (Fractional a, Eq a) => Map.Map (IGDown, IGUp, ISUp, ISDown) a ->
                                    Map.Map (IGDown, IGUp, ISUp, ISDown) a ->
                                    Map.Map (IS2Up, (ISDown, ISDown)) a ->
                                    S.Seq (PDE a)
areaPDE2 aIMap mIMap s2IMap = (\k n -> let pde1 = areaPDE2Part1Inner aIMap s2IMap k n
                                           pde2 = areaPDE2Part2Inner aIMap mIMap k n
                                           pde3 = areaPDE2Part3Inner k n
                                       in addPDEs (addPDEs pde1 pde2) pde3)
                              <$> (S.fromList spacetimeSecondRangeUp) <*> (S.fromList spacetimeRangeDown)

areaPDE3 :: (Fractional a, Eq a) => Map.Map (IGDown, IGUp, ISUp, ISDown) a ->
                                    Map.Map ((ISUp, ISUp), IS2Down) a ->
                                    Map.Map (IS3Up, (ISDown, ISDown, ISDown)) a ->
                                    S.Seq (PDE a)
areaPDE3 aIMap s2PMap s3IMap = (\k n -> areaPDE3Inner aIMap s2PMap s3IMap k n)
                               <$> (S.fromList spacetimeThirdRangeUp) <*> (S.fromList spacetimeRangeDown)

areaPDE1Part1Inner :: (Fractional a, Eq a) => Map.Map (IGDown, IGUp, ISUp, ISDown) a -> ISUp -> ISDown -> PDE a
areaPDE1Part1Inner areaIntMap m n = foldl' addPDEs (emptyPDE firstDimension) $
                                        catMaybes $
                                        (\a b -> let prefactor = areaIntMap Map.! (a, b, m, n)
                                                 in if prefactor == 0 then Nothing
                                                                      else Just $
                                                                           pdeFromConstIdepDerivative firstDimension prefactor (unIGUp b) (unIGDown a))
                                        <$> areaRangeDown <*>
                                            areaRangeUp

areaPDE1Part2Inner :: (Fractional a, Eq a) => Map.Map (IGDown, IGUp, ISUp, ISDown) a -> ISUp -> ISDown -> PDE a
areaPDE1Part2Inner areaIntMap m n = foldl' addPDEs (emptyPDE firstDimension) $
                                        catMaybes $
                                        (\a i b j -> let prefactor1 = deltaS j i * areaIntMap Map.! (a, b, m, n)
                                                         prefactor2 = -1 * deltaG b a * deltaS m i * deltaS j n
                                                         prefactor = prefactor1 + prefactor2
                                                         indicesAi = areaFirstDerivative (unIGDown a) (unISDown i)
                                                         indicesBj = areaFirstDerivative (unIGUp b) (unISUp j)
                                                     in if prefactor == 0 then Nothing
                                                                          else Just $ pdeFromConstIdepDerivative
                                                                                      firstDimension prefactor indicesBj indicesAi)
                                        <$> areaRangeDown <*>
                                            spacetimeRangeDown <*>
                                            areaRangeUp <*>
                                            spacetimeRangeUp

areaPDE1Part3Inner :: (Fractional a, Eq a) => Map.Map (IGDown, IGUp, ISUp, ISDown) a ->
                                              Map.Map (IGDown, IGUp, ISUp, ISDown) a ->
                                              ISUp -> ISDown -> PDE a
areaPDE1Part3Inner areaIntMap metricIntMap m n = foldl' addPDEs (emptyPDE firstDimension) $
                                                 catMaybes $
                                                    (\a i b j -> let prefactor1 = deltaS2 j i * areaIntMap Map.! (a, b, m, n)
                                                                     prefactor2 = deltaG b a * metricIntMap Map.! (fromIS2Down i, fromIS2Up j, m, n)
                                                                     prefactor = prefactor1 + prefactor2
                                                                     indicesAi = areaSecondDerivative (unIGDown a) (unIS2Down i)
                                                                     indicesBj = areaSecondDerivative (unIGUp b) (unIS2Up j)
                                                                 in if prefactor == 0 then Nothing
                                                                                      else Just $ pdeFromConstIdepDerivative
                                                                                                  firstDimension prefactor indicesBj indicesAi)
                                                    <$> areaRangeDown <*>
                                                        spacetimeSecondRangeDown <*>
                                                        areaRangeUp <*>
                                                        spacetimeSecondRangeUp

areaPDE2Part1Inner :: (Fractional a, Eq a) => Map.Map (IGDown, IGUp, ISUp, ISDown) a ->
                                              Map.Map (IS2Up, (ISDown, ISDown)) a ->
                                              IS2Up -> ISDown -> PDE a
areaPDE2Part1Inner areaIntMap sym2IntMap k n = foldl' addPDEs (emptyPDE firstDimension) $
                                               catMaybes $
                                                  (\a i b m -> let prefactor = areaIntMap Map.! (a, b, m, n) * sym2IntMap Map.! (k, (i, ISDown (unISUp m)))
                                                                   indicesAi = areaFirstDerivative (unIGDown a) (unISDown i)
                                                               in if prefactor == 0 then Nothing
                                                                                    else Just $ pdeFromConstIdepDerivative
                                                                                                firstDimension prefactor (unIGUp b) indicesAi)
                                                  <$> areaRangeDown <*>
                                                      spacetimeRangeDown <*>
                                                      areaRangeUp <*>
                                                      spacetimeRangeUp

areaPDE2Part2Inner :: (Fractional a, Eq a) => Map.Map (IGDown, IGUp, ISUp, ISDown) a ->
                                              Map.Map (IGDown, IGUp, ISUp, ISDown) a ->
                                              IS2Up -> ISDown -> PDE a
areaPDE2Part2Inner areaIntMap metricIntMap k n = foldl' addPDEs (emptyPDE firstDimension) $
                                                 catMaybes $
                                                    (\a i b j m -> let prefactor = areaIntMap Map.! (a, b, m, n) * (-1) *
                                                                                   metricIntMap Map.! (fromIS2Down i, fromIS2Up k, j, ISDown (unISUp m))
                                                                       indicesAi = areaSecondDerivative (unIGDown a) (unIS2Down i)
                                                                       indicesBj = areaFirstDerivative (unIGUp b) (unISUp j)
                                                                   in if prefactor == 0 then Nothing
                                                                                        else Just $ pdeFromConstIdepDerivative
                                                                                                    firstDimension prefactor indicesBj indicesAi)
                                                    <$> areaRangeDown <*>
                                                        spacetimeSecondRangeDown <*>
                                                        areaRangeUp <*>
                                                        spacetimeRangeUp <*> spacetimeRangeUp

areaPDE2Part3Inner :: (Fractional a, Eq a) => IS2Up -> ISDown -> PDE a
areaPDE2Part3Inner k n = foldl' addPDEs (emptyPDE firstDimension) $
                         catMaybes $
                            (\a -> let prefactor = -1
                                       indicesAi = areaSecondDerivative (unIGDown a) (unIS2Up k)
                                       indicesBj = areaFirstDerivative (unIGDown a) (unISDown n)
                                   in if prefactor == 0 then Nothing
                                                        else Just $ pdeFromConstIdepDerivative
                                                                    firstDimension prefactor indicesBj indicesAi)
                            <$> areaRangeDown

areaPDE3Inner :: (Fractional a, Eq a) => Map.Map (IGDown, IGUp, ISUp, ISDown) a ->
                                         Map.Map ((ISUp, ISUp), IS2Down) a ->
                                         Map.Map (IS3Up, (ISDown, ISDown, ISDown)) a ->
                                         IS3Up -> ISDown -> PDE a
areaPDE3Inner areaIntMap sym2ProjMap sym3IntMap k n = foldl' addPDEs (emptyPDE firstDimension) $
                                                      catMaybes $
                                                         (\a i b m p q -> let prefactor = areaIntMap Map.! (a, b, m, n)
                                                                                          * sym2ProjMap Map.! ((p, q), i)
                                                                                          * sym3IntMap Map.! (k, (ISDown (unISUp p),
                                                                                                                  ISDown (unISUp q),
                                                                                                                  ISDown (unISUp m)))
                                                                              indicesAi = areaSecondDerivative (unIGDown a) (unIS2Down i)
                                                                          in if prefactor == 0 then Nothing
                                                                                               else Just $ pdeFromConstIdepDerivative
                                                                                                           firstDimension prefactor (unIGUp b) indicesAi)
                                                         <$> areaRangeDown <*>
                                                             spacetimeSecondRangeDown <*>
                                                             areaRangeUp <*>
                                                             spacetimeRangeUp <*>
                                                             spacetimeRangeUp <*>
                                                             spacetimeRangeUp
