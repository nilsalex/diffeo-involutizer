module IntertwinerArea where

import qualified Data.Map.Strict as Map
import Data.Ratio

type AreaIndices = (Integer, Integer, Integer, Integer)

{- Range of area metric variables -}
areaRange :: [Integer]
areaRange = [0..20]

{- Range of spacetime dimensions -}
spacetimeRange :: [Integer]
spacetimeRange = [0..3]

{- distribution of area metric dof in area metric spacetime tensor -}
buildAreaIndicesMap :: Map.Map Integer AreaIndices
buildAreaIndicesMap = Map.fromList $ zip 
                   [0..]
                   [ (0, 1, 0, 1),
                     (0, 2, 0, 2),
                     (0, 3, 0, 3),
                     (1, 2, 1, 2),
                     (1, 3, 1, 3),
                     (2, 3, 2, 3),
                     (0, 1, 0, 2),
                     (0, 1, 0, 3),
                     (0, 1, 1, 2),
                     (0, 1, 1, 3),
                     (0, 1, 2, 3),
                     (0, 2, 0, 3),
                     (0, 2, 1, 2),
                     (0, 2, 1, 3),
                     (0, 2, 2, 3),
                     (0, 3, 1, 2),
                     (0, 3, 1, 3),
                     (0, 3, 2, 3),
                     (1, 2, 1, 3),
                     (1, 2, 2, 3),
                     (1, 3, 2, 3)  ]

{- product of five spacetime deltas -}
spacetimeDelta :: AreaIndices -> AreaIndices -> Integer -> Integer -> Rational
spacetimeDelta (a, b, c, d) (e, f, g, h) m n =
    if (m, a, b, c, d) == (e, n, f, g, h) then 1 else 0

{- factor in front of intertwiners
 - dependent on distribution of dof above -}
intertwinerFactor :: Integer -> Rational
intertwinerFactor a
    | a < 6 = -16
    | otherwise = -32

{- functions for index swapping -}
swap1 :: AreaIndices -> AreaIndices
swap1 (a, b, c, d) = (a, b, d, c)

swap2 :: AreaIndices -> AreaIndices
swap2 (a, b, c, d) = (b, a, c, d)

swap3 :: AreaIndices -> AreaIndices
swap3 (a, b, c, d) = (c, d, a, b)

{- symmetrizations by virtue of index swapping functions
 - rational prefactors are picked up -}
symmetrizeAreaIndices1 :: [(Rational, AreaIndices, AreaIndices)] -> [(Rational, AreaIndices, AreaIndices)]
symmetrizeAreaIndices1 = concat . map (\(r, a, b) -> [(r * (1 % 2), a, b), (-r * (1 % 2), swap1 a, b)])

symmetrizeAreaIndices2 :: [(Rational, AreaIndices, AreaIndices)] -> [(Rational, AreaIndices, AreaIndices)]
symmetrizeAreaIndices2 = concat . map (\(r, a, b) -> [(r * (1 % 2), a, b), (-r * (1 % 2), swap2 a, b)])

symmetrizeAreaIndices3 :: [(Rational, AreaIndices, AreaIndices)] -> [(Rational, AreaIndices, AreaIndices)]
symmetrizeAreaIndices3 = concat . map (\(r, a, b) -> [(r * (1 % 2), a, b), (r * (1 % 2), swap3 a, b)])

symmetrizeAreaIndices4 :: [(Rational, AreaIndices, AreaIndices)] -> [(Rational, AreaIndices, AreaIndices)]
symmetrizeAreaIndices4 = concat . map (\(r, a, b) -> [(r * (1 % 2), a, b), (-r * (1 % 2), a, swap1 b)])

symmetrizeAreaIndices5 :: [(Rational, AreaIndices, AreaIndices)] -> [(Rational, AreaIndices, AreaIndices)]
symmetrizeAreaIndices5 = concat . map (\(r, a, b) -> [(r * (1 % 2), a, b), (-r * (1 % 2), a, swap2 b)])

symmetrizeAreaIndices6 :: [(Rational, AreaIndices, AreaIndices)] -> [(Rational, AreaIndices, AreaIndices)]
symmetrizeAreaIndices6 = concat . map (\(r, a, b) -> [(r * (1 % 2), a, b), (r * (1 % 2), a, swap3 b)])

{- apply all symmetrizations -}
symmetrizeAreaIndices :: AreaIndices -> AreaIndices -> [(Rational, AreaIndices, AreaIndices)]
symmetrizeAreaIndices a b =( symmetrizeAreaIndices1 .
                                                     symmetrizeAreaIndices2 .
                                                     symmetrizeAreaIndices3 .
                                                     symmetrizeAreaIndices4 .
                                                     symmetrizeAreaIndices5 .
                                                     symmetrizeAreaIndices6) [(1 % 1, a, b)]

{- calculate value of spacetime Gotay-Marsden intertwiners
 - fold and inner function -}
innerIntertwiner :: Integer -> Integer -> Rational -> (Rational, AreaIndices, AreaIndices) -> Rational
innerIntertwiner m n r1 (r2, a, b) = r1 + r2 * spacetimeDelta a b m n

spacetimeIntertwiner :: AreaIndices -> AreaIndices -> Integer -> Integer -> Rational
spacetimeIntertwiner a b m n = let symAreas = symmetrizeAreaIndices a b
                                in foldl (innerIntertwiner m n) 0 symAreas

{- calculate value of Gotay-Marsden intertwiner using dof indices -}
areaIntertwiner :: Map.Map Integer AreaIndices -> Integer -> Integer -> Integer -> Integer -> Rational
areaIntertwiner indicesMap a b m n = intertwinerFactor b *
                                      spacetimeIntertwiner (indicesMap Map.! a) (indicesMap Map.! b) m n

buildAreaIntertwinerMap :: Map.Map (Integer, Integer, Integer, Integer) Rational
buildAreaIntertwinerMap = Map.fromList $ let indicesMap = buildAreaIndicesMap in
                           do a <- areaRange
                              b <- areaRange
                              m <- spacetimeRange
                              n <- spacetimeRange
                              return ((a, b, m, n), areaIntertwiner indicesMap a b m n)
