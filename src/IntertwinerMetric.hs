module IntertwinerMetric where

import Control.Parallel
import Data.Foldable
import Data.Maybe
import qualified Data.Map.Strict as Map
import Index
import Polynomial

type MetricIndices     = (Int, Int)
type MetricIndicesUp   = (ISUp, ISUp)
type MetricIndicesDown = (ISDown, ISDown)

mIUpfromMI :: MetricIndices -> MetricIndicesUp
mIUpfromMI (a, b) = (ISUp a, ISUp b)

mIDownfromMI :: MetricIndices -> MetricIndicesDown
mIDownfromMI (a, b) = (ISDown a, ISDown b)

{- Range of metric variables -}
metricRange :: [Int]
metricRange = [0..9]

metricRangeUp :: [IGUp]
metricRangeUp = map IGUp metricRange

metricRangeDown :: [IGDown]
metricRangeDown = map IGDown metricRange

{- Range of spacetime dimensions -}
spacetimeRange :: [Int]
spacetimeRange = [0..3]

spacetimeRangeUp :: [ISUp]
spacetimeRangeUp = map ISUp spacetimeRange

spacetimeRangeDown :: [ISDown]
spacetimeRangeDown = map ISDown spacetimeRange

sym2Range :: [Int]
sym2Range = metricRange

sym2RangeUp :: [IS2Up]
sym2RangeUp = map IS2Up sym2Range

sym2RangeDown :: [IS2Down]
sym2RangeDown = map IS2Down sym2Range

{- distribution of metric dof in metric spacetime tensor -}
buildMetricIndicesMap :: Map.Map Int MetricIndices
buildMetricIndicesMap = Map.fromList $ zip 
                   [0..]
                   [ (0, 0),
                     (0, 1),
                     (0, 2),
                     (0, 3),
                     (1, 1),
                     (1, 2),
                     (1, 3),
                     (2, 2),
                     (2, 3),
                     (3, 3) ]

{- product of three spacetime deltas -}
spacetimeDelta :: Num a => MetricIndicesUp -> MetricIndicesDown -> ISUp -> ISDown -> a
spacetimeDelta (a, b) (c, d) m n =
    if (unISUp m, unISUp a, unISUp b) == (unISDown c, unISDown d, unISDown n) then 1 else 0

{- factor in front of intertwiners
 - dependent on distribution of dof above -}
intertwinerFactor :: Num a => MetricIndicesUp -> a
intertwinerFactor (a, b)
    | a == b = 1
    | otherwise = 2

gmIntertwinerFactor :: Num a => a
gmIntertwinerFactor = -2

{- functions for index swapping -}
swap1 :: (a, a) -> (a, a)
swap1 (a, b) = (b, a)

{- symmetrizations by virtue of index swapping functions
 - rational prefactors are picked up -}
symmetrizeMetricIndices1 :: Fractional a => [(a, (i, i), (j, j))] -> [(a, (i, i), (j, j))]
symmetrizeMetricIndices1 = concat . map (\(r, a, b) -> [(r / 2, a, b), (r / 2, swap1 a, b)])

symmetrizeMetricIndices2 :: Fractional a => [(a, (i, i), (j, j))] -> [(a, (i, i), (j, j))]
symmetrizeMetricIndices2 = concat . map (\(r, a, b) -> [(r / 2, a, b), (r / 2, a, swap1 b)])

{- apply all symmetrizations -}
symmetrizeMetricIndices :: Fractional a => (i, i) -> (j, j) -> [(a, (i, i), (j, j))]
symmetrizeMetricIndices a b = (symmetrizeMetricIndices1 . symmetrizeMetricIndices2) [(1, a, b)]

{- calculate value of spacetime Gotay-Marsden intertwiners
 - fold and inner function -}
innerIntertwiner :: Fractional a => ISUp -> ISDown -> a -> (a, MetricIndicesDown, MetricIndicesUp) -> a
innerIntertwiner m n r1 (r2, a, b) = r1 + r2 * spacetimeDelta b a m n

spacetimeIntertwiner :: Fractional a => MetricIndicesDown -> MetricIndicesUp -> ISUp -> ISDown -> a
spacetimeIntertwiner a b m n = let symMetrics = symmetrizeMetricIndices a b
                                in foldl (innerIntertwiner m n) 0 symMetrics

{- calculate value of Gotay-Marsden intertwiner using dof indices -}
metricIntertwiner :: Fractional a => Map.Map Int MetricIndices -> IGDown -> IGUp -> ISUp -> ISDown -> a
metricIntertwiner indicesMap (IGDown a) (IGUp b) m n = gmIntertwinerFactor *
                                                       intertwinerFactor (mIUpfromMI (indicesMap Map.! b)) *
                                                        spacetimeIntertwiner (mIDownfromMI (indicesMap Map.! a))
                                                                             (mIUpfromMI (indicesMap Map.! b)) m n

buildMetricIntertwinerMap :: Fractional a => Map.Map (IGDown, IGUp, ISUp, ISDown) a
buildMetricIntertwinerMap = Map.fromList $ let indicesMap = buildMetricIndicesMap in
                             do a <- metricRangeDown
                                b <- metricRangeUp
                                m <- spacetimeRangeUp
                                n <- spacetimeRangeDown
                                return ((a, b, m, n), metricIntertwiner indicesMap a b m n)

dofIntertwiner :: Fractional a => MetricIndices -> Int -> Int -> a
dofIntertwiner (a, b) m n
                          = let summand1 = if (a == m) && (b == n) then (1 / 2) else (0)
                                summand2 = if (a == n) && (b == m) then (1 / 2) else (0)
                            in summand1 + summand2

{- calculate value of dof-to-spacetime intertwiner -}
sym2Intertwiner :: Fractional a => Map.Map Int MetricIndices -> IS2Up -> (ISDown, ISDown) -> a
sym2Intertwiner indicesMap (IS2Up a) (ISDown m, ISDown n) = intertwinerFactor (mIUpfromMI (indicesMap Map.! a)) *
                                                             dofIntertwiner (indicesMap Map.! a) m n

buildSym2IntertwinerMap :: Fractional a => Map.Map (IS2Up, (ISDown, ISDown)) a
buildSym2IntertwinerMap = Map.fromList $ let indicesMap = buildMetricIndicesMap in
                           do a <- sym2RangeUp
                              m <- spacetimeRangeDown
                              n <- spacetimeRangeDown
                              return ((a, (m, n)), sym2Intertwiner indicesMap a (m, n))

{- calculate value of spacetime-to-dof projector -}
sym2Projector :: Fractional a => Map.Map Int MetricIndices -> (ISUp, ISUp) -> IS2Down -> a
sym2Projector indicesMap (ISUp m, ISUp n) (IS2Down a) = dofIntertwiner (indicesMap Map.! a) m n

buildSym2ProjectorMap :: Fractional a => Map.Map ((ISUp, ISUp), IS2Down) a
buildSym2ProjectorMap = Map.fromList $ let indicesMap = buildMetricIndicesMap in
                                      do a <- sym2RangeDown
                                         m <- spacetimeRangeUp
                                         n <- spacetimeRangeUp
                                         return (((m, n), a), sym2Projector indicesMap (m, n) a)

permutations :: Num a => [((ISDown, ISDown, ISDown, ISDown), a)]
permutations = [
                 ((ISDown 0, ISDown 1, ISDown 2, ISDown 3), -1),
                 ((ISDown 0, ISDown 1, ISDown 3, ISDown 2), 1),
                 ((ISDown 0, ISDown 2, ISDown 1, ISDown 3), 1),
                 ((ISDown 0, ISDown 2, ISDown 3, ISDown 1), -1),
                 ((ISDown 0, ISDown 3, ISDown 1, ISDown 2), -1),
                 ((ISDown 0, ISDown 3, ISDown 2, ISDown 1), 1),
                 ((ISDown 1, ISDown 0, ISDown 2, ISDown 3), 1),
                 ((ISDown 1, ISDown 0, ISDown 3, ISDown 2), -1),
                 ((ISDown 1, ISDown 2, ISDown 0, ISDown 3), -1),
                 ((ISDown 1, ISDown 2, ISDown 3, ISDown 0), 1),
                 ((ISDown 1, ISDown 3, ISDown 0, ISDown 2), 1),
                 ((ISDown 1, ISDown 3, ISDown 2, ISDown 0), -1),
                 ((ISDown 2, ISDown 0, ISDown 1, ISDown 3), -1),
                 ((ISDown 2, ISDown 0, ISDown 3, ISDown 1), 1),
                 ((ISDown 2, ISDown 1, ISDown 0, ISDown 3), 1),
                 ((ISDown 2, ISDown 1, ISDown 3, ISDown 0), -1),
                 ((ISDown 2, ISDown 3, ISDown 0, ISDown 1), -1),
                 ((ISDown 2, ISDown 3, ISDown 1, ISDown 0), 1),
                 ((ISDown 3, ISDown 0, ISDown 1, ISDown 2), 1),
                 ((ISDown 3, ISDown 0, ISDown 2, ISDown 1), -1),
                 ((ISDown 3, ISDown 1, ISDown 0, ISDown 2), -1),
                 ((ISDown 3, ISDown 1, ISDown 2, ISDown 0), 1),
                 ((ISDown 3, ISDown 2, ISDown 0, ISDown 1), 1),
                 ((ISDown 3, ISDown 2, ISDown 1, ISDown 0), -1)
               ]

metricEpsilonIntertwiner :: Fractional a => Map.Map (IS2Up, (ISDown, ISDown)) a ->
                                            IGUp -> IGUp -> IGUp -> IGUp
                                            -> a
metricEpsilonIntertwiner imap gA gB gC gD = sum $(\((a, b, c, d), f1) ((p, q, r, s), f2) ->
                                                    (1/24) * f1 * f2 *
                                                    (imap Map.! (fromIGUp gA, (a, p))) *
                                                    (imap Map.! (fromIGUp gB, (b, q))) *
                                                    (imap Map.! (fromIGUp gC, (c, r))) *
                                                    (imap Map.! (fromIGUp gD, (d, s)))
                                                   ) <$>
                                                     permutations <*>
                                                     permutations

buildMetricEpsilonMap :: Fractional a => Map.Map (IGUp, IGUp, IGUp, IGUp) a
buildMetricEpsilonMap = Map.fromList $ let intertwinerMap = buildSym2IntertwinerMap
                                       in (\a b c d ->
                                            ((a, b, c, d), metricEpsilonIntertwiner intertwinerMap a b c d)) <$>
                                              metricRangeUp <*>
                                              metricRangeUp <*>
                                              metricRangeUp <*>
                                              metricRangeUp

metricDeterminant :: (Fractional a, Eq a) => Polynomial a
metricDeterminant = let epsilon = buildMetricEpsilonMap
                    in foldl' addPolynomials (emptyPolynomial 10) $
                       catMaybes $
                       (\a@(IGUp i) b@(IGUp j) c@(IGUp k) d@(IGUp l)
                                 -> let coefficient = epsilon Map.! (a, b, c, d)
                                    in if coefficient == 0 then Nothing
                                                           else Just $ quartic 10 i j k l coefficient)
                       <$>
                         metricRangeUp <*>
                         metricRangeUp <*>
                         metricRangeUp <*>
                         metricRangeUp

ricciIntertwiner1 :: Fractional a => Map.Map (IS2Up, (ISDown, ISDown)) a ->
                                     IGUp -> IS2Up -> IGDown -> IGDown -> a
ricciIntertwiner1 imap (IGUp gA) (IS2Up gB) (IGDown gC) (IGDown gD)
                    = sum $
                      (\a b c d -> let iabA = imap Map.! (IS2Up gA, (ISDown a, ISDown b))
                                       icdB = imap Map.! (IS2Up gB, (ISDown c, ISDown d))
                                       iabC = imap Map.! (IS2Up gC, (ISDown a, ISDown b))
                                       icdD = imap Map.! (IS2Up gD, (ISDown c, ISDown d))
                                       iacC = imap Map.! (IS2Up gC, (ISDown a, ISDown c))
                                       ibdD = imap Map.! (IS2Up gD, (ISDown b, ISDown d))
                                   in (-1) * iabA * icdB * iabC * icdD + iabA * icdB * iacC * ibdD)
                           <$>
                           spacetimeRange <*>
                           spacetimeRange <*>
                           spacetimeRange <*>
                           spacetimeRange

buildRicciIntertwiner1Map :: Fractional a => Map.Map (IGUp, IS2Up, IGDown, IGDown) a
buildRicciIntertwiner1Map = Map.fromList $
                            (\gA gB gC gD -> ((gA, gB, gC, gD), ricciIntertwiner1 imap gA gB gC gD))
                            <$> metricRangeUp <*>
                                sym2RangeUp <*>
                                metricRangeDown <*>
                                metricRangeDown
                            where imap = buildSym2IntertwinerMap

ricciIntertwiner2 :: Fractional a => Map.Map (IS2Up, (ISDown, ISDown)) a ->
                                     IGUp -> IGUp -> ISUp -> ISUp ->
                                     IGDown -> IGDown -> IGDown -> a
ricciIntertwiner2 imap (IGUp gA) (IGUp gB) (ISUp a) (ISUp b)
                       (IGDown gC) (IGDown gD) (IGDown gE)
                    = sum $
                      (\a b c d e f -> let icdA = imap Map.! (IS2Up gA, (ISDown c, ISDown d))
                                           iefB = imap Map.! (IS2Up gB, (ISDown e, ISDown f))
                                           iabC = imap Map.! (IS2Up gC, (ISDown a, ISDown b))
                                           icdD = imap Map.! (IS2Up gD, (ISDown c, ISDown d))
                                           iefE = imap Map.! (IS2Up gE, (ISDown e, ISDown f))
                                           iceD = imap Map.! (IS2Up gD, (ISDown c, ISDown e))
                                           idfE = imap Map.! (IS2Up gE, (ISDown d, ISDown f))
                                           iacC = imap Map.! (IS2Up gC, (ISDown a, ISDown c))
                                           ibdD = imap Map.! (IS2Up gD, (ISDown b, ISDown d))
                                           ideD = imap Map.! (IS2Up gD, (ISDown d, ISDown e))
                                           ibfE = imap Map.! (IS2Up gE, (ISDown b, ISDown f))
                                           iaeC = imap Map.! (IS2Up gC, (ISDown a, ISDown e))
                                           ibcD = imap Map.! (IS2Up gD, (ISDown b, ISDown c))
                                       in icdA * iefB *
                                          ( (-1/4) * iabC * icdD * iefE
                                          +  (3/4) * iabC * iceD * idfE
                                          +          iacC * ibdD * iefE
                                          -          iacC * ideD * ibfE
                                          -  (1/2) * iaeC * ibcD * idfE))
                           <$>
                           spacetimeRange <*>
                           spacetimeRange <*>
                           spacetimeRange <*>
                           spacetimeRange <*>
                           spacetimeRange <*>
                           spacetimeRange

buildRicciIntertwiner2Map :: Fractional a => Map.Map (IGUp, IGUp, ISUp, ISUp, IGDown, IGDown, IGDown) a
buildRicciIntertwiner2Map = Map.fromList $
                            (\gA gB a b gC gD gE-> ((gA, gB, a, b, gC, gD, gE),
                                                    ricciIntertwiner2 imap gA gB a b gC gD gE))
                            <$> metricRangeUp <*>
                                metricRangeUp <*>
                                spacetimeRangeUp <*>
                                spacetimeRangeUp <*>
                                metricRangeDown <*>
                                metricRangeDown <*>
                                metricRangeDown
                            where imap = buildSym2IntertwinerMap

ricci1 :: (Fractional a, Eq a) => Polynomial a
ricci1 = foldl' addPolynomials (emptyPolynomial 160) $
         catMaybes $
         (\gA@(IGUp a) gB@(IS2Up b) gC@(IGDown c) gD@(IGDown d) ->
                let coefficient = ricciIntertwiner1 imap gA gB gC gD
                in if coefficient == 0 then Nothing
                                       else Just $ monomial 160 [50+10*a+b, 150+c, 150+d] coefficient)
         <$> metricRangeUp <*>
             sym2RangeUp <*>
             metricRangeDown <*>
             metricRangeDown
      where imap = buildSym2IntertwinerMap

ricci2 :: (Fractional a, Eq a) => Polynomial a
ricci2 = let a = ricci2p $ map IGUp [0, 1]
             b = ricci2p $ map IGUp [2, 3]
             c = ricci2p $ map IGUp [4, 5]
             d = ricci2p $ map IGUp [6, 7]
             e = ricci2p $ map IGUp [8, 9]
         in a `par` b `par` c `par` d `par` e `pseq` addPolynomials (addPolynomials (addPolynomials (addPolynomials a b) c) d) e

ricci2p :: (Fractional a, Eq a) => [IGUp] -> Polynomial a
ricci2p range = foldl' addPolynomials (emptyPolynomial 160) $
                catMaybes $
                (\gA@(IGUp aA) gB@(IGUp bB) sA@(ISUp a) sB@(ISUp b) gC@(IGDown c) gD@(IGDown d) gE@(IGDown e) ->
                       let coefficient = ricciIntertwiner2 imap gA gB sA sB gC gD gE
                       in if coefficient == 0 then Nothing
                                              else Just $ monomial 160 [10+4*aA+a, 10+4*bB+b, 150+c, 150+d, 150+e] coefficient)
                <$> range <*>
                    metricRangeUp <*>
                    spacetimeRangeUp <*>
                    spacetimeRangeUp <*>
                    metricRangeDown <*>
                    metricRangeDown <*>
                    metricRangeDown
             where imap = buildSym2IntertwinerMap
