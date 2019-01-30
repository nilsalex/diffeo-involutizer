module DiffeoEquationsArea (areaPDE,
                            evalAreaAtEta,
                            areaExInt1,
                            areaExInt2,
                            areaExInt3,
                            ansatz1, ansatz2, ansatz3,
                            ansatz1inner, ansatz2inner, ansatz3inner)
where

import PDE
import qualified IntertwinerArea
import qualified IntertwinerMetric
import qualified IntertwinerSym3
import qualified IntertwinerASym2
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
areaDimension = 21

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

epsilon :: Num a => Int -> Int -> Int -> Int -> a
epsilon 0 1 2 3 = 1
epsilon 0 1 3 2 = -1
epsilon 0 2 1 3 = -1
epsilon 0 2 3 1 = 1
epsilon 0 3 1 2 = 1
epsilon 0 3 2 1 = -1
epsilon 1 0 2 3 = -1
epsilon 1 0 3 2 = 1
epsilon 1 2 0 3 = 1
epsilon 1 2 3 0 = -1
epsilon 1 3 0 2 = -1
epsilon 1 3 2 0 = 1
epsilon 2 0 1 3 = 1
epsilon 2 0 3 1 = -1
epsilon 2 1 0 3 = -1
epsilon 2 1 3 0 = 1
epsilon 2 3 0 1 = 1
epsilon 2 3 1 0 = -1
epsilon 3 0 1 2 = -1
epsilon 3 0 2 1 = 1
epsilon 3 1 0 2 = 1
epsilon 3 1 2 0 = -1
epsilon 3 2 0 1 = -1
epsilon 3 2 1 0 = 1
epsilon _ _ _ _ = 0

epsilonSDown :: Num a => ISDown -> ISDown -> ISDown -> ISDown -> a
epsilonSDown (ISDown a) (ISDown b) (ISDown c) (ISDown d) = epsilon a b c d

epsilonSUp :: Num a => ISUp -> ISUp -> ISUp -> ISUp -> a
epsilonSUp (ISUp a) (ISUp b) (ISUp c) (ISUp d) = epsilon a b c d

eta :: Num a => Int -> Int -> a
eta 0 0 = 1
eta 1 1 = -1
eta 2 2 = -1
eta 3 3 = -1
eta _ _ = 0

etaSUp :: Num a => ISUp -> ISUp -> a
etaSUp (ISUp a) (ISUp b) = eta a b

etaSDown :: Num a => ISDown -> ISDown -> a
etaSDown (ISDown a) (ISDown b) = eta a b

areaFirstDerivative :: Int -> Int -> Int
areaFirstDerivative a i = areaDimension + a * spacetimeDimension + i

areaSecondDerivative :: Int -> Int -> Int
areaSecondDerivative a i = (spacetimeDimension + 1) * areaDimension + a * spacetimeSecondDimension + i

firstDimension :: Int
firstDimension = (1 + spacetimeDimension + spacetimeSecondDimension) * areaDimension

firstRange :: [Int]
firstRange = [0..firstDimension - 1]

buildEtaEtaMap :: (Num a, Eq a) => Map.Map Int a
--buildEtaEtaMap = Map.fromList $ zip firstRange $ [-1, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, -1, 0, 0, 0, 1, 0, 0, 1, 0, 1] ++ repeat 0
buildEtaEtaMap = Map.fromList $ zip firstRange $ [-1, 0, 0, 0, 0, -1, -1, 0, 0, 1, 0, -1, -1, 0, 0, 1, 0, 0, 1, 0, 1] ++ repeat 0

evalAreaAtEta :: (Num a, Eq a, Fractional a) => PDESystem a -> PDESystem a
evalAreaAtEta = evalPDESystem buildEtaEtaMap

areaPDE :: (Fractional a, Eq a) => PDESystem a
areaPDE = runEval $
          do
            pde1' <- parTraversable rpar pde1
            pde2' <- parTraversable rpar pde2
            pde3' <- parTraversable rpar pde3
            return $ PDESys firstDimension $
                             pde1' S.><
                             pde2' S.><
                             pde3'
           where
               aIMap = IntertwinerArea.buildAreaIntertwinerMap
               mIMap = IntertwinerMetric.buildMetricIntertwinerMap
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

areaExInt1 :: (Fractional a, Eq a) => PDESystem a
areaExInt1 = PDESys firstDimension $
             (\mM aA -> let pde1 = areaExInt1Part1Inner aIMap s3pMap mM aA
                        in pde1)
             <$> (S.fromList IntertwinerASym2.asym2RangeDown) <*>
                 (S.fromList areaRangeUp)
    where 
      aIMap = IntertwinerArea.buildAreaIntertwinerMap
      s3pMap = IntertwinerASym2.buildASym2ProjectorMap

areaExInt2 :: (Fractional a, Eq a) => PDESystem a
areaExInt2 = PDESys firstDimension $
             (\mM aA p -> let pde1 = areaExInt2Part1Inner s3pMap mM aA p
                              pde2 = areaExInt2Part2Inner aIMap s3pMap mM aA p
                          in addPDEs pde1 pde2)
             <$> (S.fromList IntertwinerASym2.asym2RangeDown) <*>
                 (S.fromList areaRangeUp) <*>
                 (S.fromList spacetimeRangeUp)
    where 
      aIMap = IntertwinerArea.buildAreaIntertwinerMap
      s3pMap = IntertwinerASym2.buildASym2ProjectorMap

areaExInt3 :: (Fractional a, Eq a) => PDESystem a
areaExInt3 = PDESys firstDimension $
             (\mM aA iI -> let pde1 = areaExInt3Part1Inner aIMap s3pMap mM aA iI
                               pde2 = areaExInt3Part2Inner mIMap s3pMap mM aA iI
                           in addPDEs pde1 pde2)
             <$> (S.fromList IntertwinerASym2.asym2RangeDown) <*>
                 (S.fromList areaRangeUp) <*>
                 (S.fromList spacetimeSecondRangeDown)
    where 
      aIMap = IntertwinerArea.buildAreaIntertwinerMap
      mIMap = IntertwinerMetric.buildMetricIntertwinerMap
      s3pMap = IntertwinerASym2.buildASym2ProjectorMap

areaExInt1Part1Inner :: (Fractional a, Eq a) => Map.Map (IGDown, IGUp, ISUp, ISDown) a ->
                                                Map.Map ((ISUp, ISUp), IAS2Down) a ->
                                                IAS2Down -> IGUp -> PDE a
areaExInt1Part1Inner aIMap s3pMap mM aA = foldl' addPDEs (emptyPDE firstDimension) $
                                                 catMaybes $
                                                 (\bB m n i -> let fac1 = s3pMap Map.! ((m, n), mM)
                                                                   fac2 = etaSUp n (ISUp $ unISDown i)
                                                                   fac3 = aIMap Map.! (bB, aA, m, i)
                                                                   factor = fac1 * fac2 * fac3
                                                               in if factor == 0 then Nothing
                                                                                 else Just $
                                         pdeFromConstDerivative firstDimension factor (unIGDown bB))
                                                 <$> areaRangeDown <*>
                                                     spacetimeRangeUp <*>
                                                     spacetimeRangeUp <*>
                                                     spacetimeRangeDown

areaExInt2Part1Inner :: (Fractional a, Eq a) => Map.Map ((ISUp, ISUp), IAS2Down) a -> IAS2Down -> IGUp -> ISUp -> PDE a
areaExInt2Part1Inner s3pMap mM aA p = foldl' addPDEs (emptyPDE firstDimension) $
                                             catMaybes $
                                             (\m n -> let fac1 = s3pMap Map.! ((m, n), mM)
                                                          fac2 = etaSUp n p
                                                          factor = fac1 * fac2
                                                          indicesAm = areaFirstDerivative (unIGUp aA) (unISUp m)
                                                      in if factor == 0 then Nothing
                                                                        else Just $
                                     pdeFromConstDerivative firstDimension factor indicesAm)
                                             <$> spacetimeRangeUp <*>
                                                 spacetimeRangeUp

areaExInt2Part2Inner :: (Fractional a, Eq a) => Map.Map (IGDown, IGUp, ISUp, ISDown) a ->
                                                Map.Map ((ISUp, ISUp), IAS2Down) a ->
                                                IAS2Down -> IGUp -> ISUp -> PDE a
areaExInt2Part2Inner aIMap s3pMap mM aA p = foldl' addPDEs (emptyPDE firstDimension) $
                                                   catMaybes $
                                                   (\bB m n i -> let fac1 = s3pMap Map.! ((m, n), mM)
                                                                     fac2 = etaSUp n (ISUp $ unISDown i)
                                                                     fac3 = aIMap Map.! (bB, aA, m, i)
                                                                     factor = (-1) * fac1 * fac2 * fac3
                                                                     indicesBp = areaFirstDerivative (unIGDown bB) (unISUp p)
                                                                 in if factor == 0 then Nothing
                                                                                   else Just $
                                           pdeFromConstDerivative firstDimension factor indicesBp)
                                                   <$> areaRangeDown <*>
                                                       spacetimeRangeUp <*>
                                                       spacetimeRangeUp <*>
                                                       spacetimeRangeDown

areaExInt3Part1Inner :: (Fractional a, Eq a) => Map.Map (IGDown, IGUp, ISUp, ISDown) a ->
                                                Map.Map ((ISUp, ISUp), IAS2Down) a ->
                                                IAS2Down -> IGUp -> IS2Down -> PDE a
areaExInt3Part1Inner aIMap s3pMap mM aA iI = foldl' addPDEs (emptyPDE firstDimension) $
                                                    catMaybes $
                                                    (\bB m n i -> let fac1 = s3pMap Map.! ((m, n), mM)
                                                                      fac2 = etaSUp n (ISUp $ unISDown i)
                                                                      fac3 = aIMap Map.! (bB, aA, m, i)
                                                                      factor = fac1 * fac2 * fac3
                                                                      indicesBI = areaSecondDerivative (unIGDown bB) (unIS2Down iI)
                                                                  in if factor == 0 then Nothing
                                                                                    else Just $
                                            pdeFromConstDerivative firstDimension factor indicesBI)
                                                    <$> areaRangeDown <*>
                                                        spacetimeRangeUp <*>
                                                        spacetimeRangeUp <*>
                                                        spacetimeRangeDown

areaExInt3Part2Inner :: (Fractional a, Eq a) => Map.Map (IGDown, IGUp, ISUp, ISDown) a ->
                                                Map.Map ((ISUp, ISUp), IAS2Down) a ->
                                                IAS2Down -> IGUp -> IS2Down -> PDE a
areaExInt3Part2Inner mIMap s3pMap mM aA iI = foldl' addPDEs (emptyPDE firstDimension) $
                                                    catMaybes $
                                                    (\jJ m n i -> let fac1 = s3pMap Map.! ((m, n), mM)
                                                                      fac2 = etaSUp n (ISUp $ unISDown i)
                                                                      fac3 = mIMap Map.! (fromIS2Down iI, fromIS2Up jJ, m, i)
                                                                      factor = fac1 * fac2 * fac3
                                                                      indicesAJ = areaSecondDerivative (unIGUp aA) (unIS2Up jJ)
                                                                  in if factor == 0 then Nothing
                                                                                    else Just $
                                            pdeFromConstDerivative firstDimension factor indicesAJ)
                                                    <$> spacetimeSecondRangeUp <*>
                                                        spacetimeRangeUp <*>
                                                        spacetimeRangeUp <*>
                                                        spacetimeRangeDown

ansatz1inner :: (Fractional a, Eq a) => ISDown -> ISDown -> ISDown -> ISDown -> ISDown -> ISDown -> a
ansatz1inner a b c d p q = 1/2 * (etaSDown p q) * ((etaSDown a c) * (etaSDown b d) - (etaSDown a d) * (etaSDown b c))

ansatz2inner :: (Fractional a, Eq a) => ISDown -> ISDown -> ISDown -> ISDown -> ISDown -> ISDown -> a
ansatz2inner a b c d p q = 1/8 * (   (etaSDown p a) * (etaSDown b c) * (etaSDown d q)
                                   - (etaSDown p b) * (etaSDown a c) * (etaSDown d q)
                                   - (etaSDown p a) * (etaSDown b d) * (etaSDown c q)
                                   + (etaSDown q a) * (etaSDown b c) * (etaSDown d p)
                                   + (etaSDown p b) * (etaSDown a d) * (etaSDown c q)
                                   - (etaSDown q b) * (etaSDown a c) * (etaSDown d p)
                                   - (etaSDown q a) * (etaSDown b d) * (etaSDown c p)
                                   + (etaSDown q b) * (etaSDown a d) * (etaSDown c p) )

ansatz3inner :: (Fractional a, Eq a) => ISDown -> ISDown -> ISDown -> ISDown -> ISDown -> ISDown -> a
ansatz3inner a b c d p q = (epsilonSDown a b c d) * (etaSDown p q)

ansatzAI :: (Fractional a, Eq a) => (ISDown -> ISDown -> ISDown -> ISDown -> ISDown -> ISDown -> a) ->
                                    IGUp -> IS2Up -> a
ansatzAI f aA iI = foldl' (+) 0 $
                   (\a b c d p q -> let fac1 = f a b c d p q
                                        fac2 = aIMap Map.! (aA, (a, b, c, d))
                                        fac3 = s2IMap Map.! (iI, (p, q))
                                    in fac1 * fac2 * fac3)
                   <$> spacetimeRangeDown <*>
                       spacetimeRangeDown <*>
                       spacetimeRangeDown <*>
                       spacetimeRangeDown <*>
                       spacetimeRangeDown <*>
                       spacetimeRangeDown
  where
    aIMap = IntertwinerArea.buildAreaDofIntertwinerMap
    s2IMap = IntertwinerMetric.buildSym2IntertwinerMap

ansatz1 :: (Fractional a, Eq a) => [a]
ansatz1 = ansatzAI ansatz1inner <$> areaRangeUp <*> spacetimeSecondRangeUp

ansatz2 :: (Fractional a, Eq a) => [a]
ansatz2 = ansatzAI ansatz2inner <$> areaRangeUp <*> spacetimeSecondRangeUp

ansatz3 :: (Fractional a, Eq a) => [a]
ansatz3 = ansatzAI ansatz3inner <$> areaRangeUp <*> spacetimeSecondRangeUp
