module IntertwinerMetric where

import qualified Data.Map.Strict as Map
import Index

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

