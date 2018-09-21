module IntertwinerArea where

import qualified Data.Map.Strict as Map

type AreaIndices = (Int, Int, Int, Int)

{- Range of area metric variables -}
areaRange :: [Int]
areaRange = [0..20]

{- Range of spacetime dimensions -}
spacetimeRange :: [Int]
spacetimeRange = [0..3]

{- distribution of area metric dof in area metric spacetime tensor -}
buildAreaIndicesMap :: Map.Map Int AreaIndices
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
spacetimeDelta :: Num a => AreaIndices -> AreaIndices -> Int -> Int -> a
spacetimeDelta (a, b, c, d) (e, f, g, h) m n =
    if (m, a, b, c, d) == (e, n, f, g, h) then 1 else 0

{- factor in front of intertwiners
 - dependent on distribution of dof above -}
intertwinerFactor :: Num a => Int -> a
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
symmetrizeAreaIndices1 :: Fractional a => [(a, AreaIndices, AreaIndices)] -> [(a, AreaIndices, AreaIndices)]
symmetrizeAreaIndices1 = concat . map (\(r, a, b) -> [(r / 2, a, b), (-r / 2, swap1 a, b)])

symmetrizeAreaIndices2 :: Fractional a => [(a, AreaIndices, AreaIndices)] -> [(a, AreaIndices, AreaIndices)]
symmetrizeAreaIndices2 = concat . map (\(r, a, b) -> [(r / 2, a, b), (-r / 2, swap2 a, b)])

symmetrizeAreaIndices3 :: Fractional a => [(a, AreaIndices, AreaIndices)] -> [(a, AreaIndices, AreaIndices)]
symmetrizeAreaIndices3 = concat . map (\(r, a, b) -> [(r / 2, a, b), (r / 2, swap3 a, b)])

symmetrizeAreaIndices4 :: Fractional a => [(a, AreaIndices, AreaIndices)] -> [(a, AreaIndices, AreaIndices)]
symmetrizeAreaIndices4 = concat . map (\(r, a, b) -> [(r / 2, a, b), (-r / 2, a, swap1 b)])

symmetrizeAreaIndices5 :: Fractional a => [(a, AreaIndices, AreaIndices)] -> [(a, AreaIndices, AreaIndices)]
symmetrizeAreaIndices5 = concat . map (\(r, a, b) -> [(r / 2, a, b), (-r / 2, a, swap2 b)])

symmetrizeAreaIndices6 :: Fractional a => [(a, AreaIndices, AreaIndices)] -> [(a, AreaIndices, AreaIndices)]
symmetrizeAreaIndices6 = concat . map (\(r, a, b) -> [(r / 2, a, b), (r / 2, a, swap3 b)])

{- apply all symmetrizations -}
symmetrizeAreaIndices :: Fractional a => AreaIndices -> AreaIndices -> [(a, AreaIndices, AreaIndices)]
symmetrizeAreaIndices a b =( symmetrizeAreaIndices1 .
                                                     symmetrizeAreaIndices2 .
                                                     symmetrizeAreaIndices3 .
                                                     symmetrizeAreaIndices4 .
                                                     symmetrizeAreaIndices5 .
                                                     symmetrizeAreaIndices6) [(1, a, b)]

{- calculate value of spacetime Gotay-Marsden intertwiners
 - fold and inner function -}
innerIntertwiner :: Fractional a => Int -> Int -> a -> (a, AreaIndices, AreaIndices) -> a
innerIntertwiner m n r1 (r2, a, b) = r1 + r2 * spacetimeDelta a b m n

spacetimeIntertwiner :: Fractional a => AreaIndices -> AreaIndices -> Int -> Int -> a
spacetimeIntertwiner a b m n = let symAreas = symmetrizeAreaIndices a b
                                in foldl (innerIntertwiner m n) 0 symAreas

{- calculate value of Gotay-Marsden intertwiner using dof indices -}
areaIntertwiner :: Fractional a => Map.Map Int AreaIndices -> Int -> Int -> Int -> Int -> a
areaIntertwiner indicesMap a b m n = intertwinerFactor b *
                                      spacetimeIntertwiner (indicesMap Map.! a) (indicesMap Map.! b) m n

buildAreaIntertwinerMap :: Fractional a => Map.Map (Int, Int, Int, Int) a
buildAreaIntertwinerMap = Map.fromList $ let indicesMap = buildAreaIndicesMap in
                           do a <- areaRange
                              b <- areaRange
                              m <- spacetimeRange
                              n <- spacetimeRange
                              return ((a, b, m, n), areaIntertwiner indicesMap a b m n)
