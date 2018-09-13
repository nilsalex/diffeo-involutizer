module IntertwinerMetric where

import qualified Data.Map.Strict as Map
import Data.Ratio

type MetricIndices = (Integer, Integer)

{- Range of metric variables -}
metricRange :: [Integer]
metricRange = [0..9]

{- Range of spacetime dimensions -}
spacetimeRange :: [Integer]
spacetimeRange = [0..3]

{- distribution of metric dof in metric spacetime tensor -}
buildMetricIndicesMap :: Map.Map Integer MetricIndices
buildMetricIndicesMap = Map.fromList $ zip 
                   [0..]
                   [ (0, 0),
                     (1, 1),
                     (2, 2),
                     (3, 3),
                     (0, 1),
                     (0, 2),
                     (0, 3),
                     (1, 2),
                     (1, 3),
                     (2, 3) ]

{- product of three spacetime deltas -}
spacetimeDelta :: MetricIndices -> MetricIndices -> Integer -> Integer -> Rational
spacetimeDelta (a, b) (c, d) m n =
    if (m, a, b) == (c, d, n) then 1 else 0

{- factor in front of intertwiners
 - dependent on distribution of dof above -}
intertwinerFactor :: Integer -> Rational
intertwinerFactor a
    | a < 4 = 1
    | otherwise = 2

{- functions for index swapping -}
swap1 :: MetricIndices -> MetricIndices
swap1 (a, b) = (b, a)

{- symmetrizations by virtue of index swapping functions
 - rational prefactors are picked up -}
symmetrizeMetricIndices1 :: [(Rational, MetricIndices, MetricIndices)] -> [(Rational, MetricIndices, MetricIndices)]
symmetrizeMetricIndices1 = concat . map (\(r, a, b) -> [(r * (1 % 2), a, b), (r * (1 % 2), swap1 a, b)])

symmetrizeMetricIndices2 :: [(Rational, MetricIndices, MetricIndices)] -> [(Rational, MetricIndices, MetricIndices)]
symmetrizeMetricIndices2 = concat . map (\(r, a, b) -> [(r * (1 % 2), a, b), (r * (1 % 2), a, swap1 b)])

{- apply all symmetrizations -}
symmetrizeMetricIndices :: MetricIndices -> MetricIndices -> [(Rational, MetricIndices, MetricIndices)]
symmetrizeMetricIndices a b = (symmetrizeMetricIndices1 . symmetrizeMetricIndices2) [(1 % 1, a, b)]

{- calculate value of spacetime Gotay-Marsden intertwiners
 - fold and inner function -}
innerIntertwiner :: Integer -> Integer -> Rational -> (Rational, MetricIndices, MetricIndices) -> Rational
innerIntertwiner m n r1 (r2, a, b) = r1 + r2 * spacetimeDelta a b m n

spacetimeIntertwiner :: MetricIndices -> MetricIndices -> Integer -> Integer -> Rational
spacetimeIntertwiner a b m n = let symMetrics = symmetrizeMetricIndices a b
                                in foldl (innerIntertwiner m n) 0 symMetrics

{- calculate value of Gotay-Marsden intertwiner using dof indices -}
metricIntertwiner :: Map.Map Integer MetricIndices -> Integer -> Integer -> Integer -> Integer -> Rational
metricIntertwiner indicesMap a b m n = intertwinerFactor b *
                                        spacetimeIntertwiner (indicesMap Map.! a) (indicesMap Map.! b) m n

buildMetricIntertwinerMap :: Map.Map (Integer, Integer, Integer, Integer) Rational
buildMetricIntertwinerMap = Map.fromList $ let indicesMap = buildMetricIndicesMap in
                             do a <- metricRange
                                b <- metricRange
                                m <- spacetimeRange
                                n <- spacetimeRange
                                return ((a, b, m, n), metricIntertwiner indicesMap a b m n)

dofIntertwiner :: MetricIndices -> Integer -> Integer -> Rational
dofIntertwiner (a, b) m n = let summand1 = if (a == m) && (b == n) then (1 % 2) else (0 % 1)
                                summand2 = if (a == n) && (b == m) then (1 % 2) else (0 % 1)
                            in summand1 + summand2

{- calculate value of dof-to-spacetime intertwiner -}
sym2Intertwiner :: Map.Map Integer MetricIndices -> Integer -> (Integer, Integer) -> Rational
sym2Intertwiner indicesMap a (m, n) = intertwinerFactor a * dofIntertwiner (indicesMap Map.! a) m n

buildSym2IntertwinerMap :: Map.Map (Integer, (Integer, Integer)) Rational
buildSym2IntertwinerMap = Map.fromList $ let indicesMap = buildMetricIndicesMap in
                           do a <- metricRange
                              m <- spacetimeRange
                              n <- spacetimeRange
                              return ((a, (m, n)), sym2Intertwiner indicesMap a (m, n))

{- calculate value of spacetime-to-dof projector -}
sym2Projector :: Map.Map Integer MetricIndices -> (Integer, Integer) -> Integer -> Rational
sym2Projector indicesMap (m, n) a = dofIntertwiner (indicesMap Map.! a) m n

buildSym2ProjectorMap :: Map.Map ((Integer, Integer), Integer) Rational
buildSym2ProjectorMap = Map.fromList $ let indicesMap = buildMetricIndicesMap in
                                      do a <- metricRange
                                         m <- spacetimeRange
                                         n <- spacetimeRange
                                         return (((m, n), a), sym2Projector indicesMap (m, n) a)

