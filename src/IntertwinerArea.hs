module IntertwinerArea where

import qualified Data.Map.Strict as Map
import Index

type AreaIndices     = (Int, Int, Int, Int)
type AreaIndicesUp   = (ISUp, ISUp, ISUp, ISUp)
type AreaIndicesDown = (ISDown, ISDown, ISDown, ISDown)

aIUpfromAI :: AreaIndices -> AreaIndicesUp
aIUpfromAI (a, b, c, d) = (ISUp a, ISUp b, ISUp c, ISUp d)

aIDownfromAI :: AreaIndices -> AreaIndicesDown
aIDownfromAI (a, b, c, d) = (ISDown a, ISDown b, ISDown c, ISDown d)

{- Range of area metric variables -}
areaRange :: [Int]
areaRange = [0..20]

areaRangeUp :: [IGUp]
areaRangeUp = map IGUp areaRange

areaRangeDown :: [IGDown]
areaRangeDown = map IGDown areaRange

{- Range of spacetime dimensions -}
spacetimeRange :: [Int]
spacetimeRange = [0..3]

spacetimeRangeUp :: [ISUp]
spacetimeRangeUp = map ISUp spacetimeRange

spacetimeRangeDown :: [ISDown]
spacetimeRangeDown = map ISDown spacetimeRange

{- distribution of area metric dof in area metric spacetime tensor -}
buildAreaIndicesMap :: Map.Map Int AreaIndices
buildAreaIndicesMap = Map.fromList $ zip 
                   [0..]
                   [ (0, 1, 0, 1),
                     (0, 1, 0, 2),
                     (0, 1, 0, 3),
                     (0, 1, 1, 2),
                     (0, 1, 1, 3),
                     (0, 1, 2, 3),
                     (0, 2, 0, 2),
                     (0, 2, 0, 3),
                     (0, 2, 1, 2),
                     (0, 2, 1, 3),
                     (0, 2, 2, 3),
                     (0, 3, 0, 3),
                     (0, 3, 1, 2),
                     (0, 3, 1, 3),
                     (0, 3, 2, 3),
                     (1, 2, 1, 2),
                     (1, 2, 1, 3),
                     (1, 2, 2, 3),
                     (1, 3, 1, 3),
                     (1, 3, 2, 3),
                     (2, 3, 2, 3) ]

{- product of five spacetime deltas -}
spacetimeDelta :: Num a => AreaIndicesUp -> AreaIndicesDown -> ISUp -> ISDown -> a
spacetimeDelta (a, b, c, d) (e, f, g, h) m n =
    if (unISUp m, unISUp a, unISUp b, unISUp c, unISUp d) ==
       (unISDown e, unISDown n, unISDown f, unISDown g, unISDown h) then 1 else 0

{- factor in front of intertwiners
 - dependent on distribution of dof above -}
intertwinerFactor :: Num a => AreaIndicesUp -> a
intertwinerFactor (i, j, k, l)
        | i == k && j == l = -16
        | otherwise = -32
        

{- functions for index swapping -}
swap1 :: (a, a, a, a) -> (a, a, a, a)
swap1 (a, b, c, d) = (a, b, d, c)

swap2 :: (a, a, a, a) -> (a, a, a, a)
swap2 (a, b, c, d) = (b, a, c, d)

swap3 :: (a, a, a, a) -> (a, a, a, a)
swap3 (a, b, c, d) = (c, d, a, b)

{- symmetrizations by virtue of index swapping functions
 - rational prefactors are picked up -}
symmetrizeAreaIndices1 :: Fractional a => [(a, (i, i, i, i), (j, j, j, j))] -> [(a, (i, i, i, i), (j, j, j, j))]
symmetrizeAreaIndices1 = concat . map (\(r, a, b) -> [(r / 2, a, b), (-r / 2, swap1 a, b)])

symmetrizeAreaIndices2 :: Fractional a => [(a, (i, i, i, i), (j, j, j, j))] -> [(a, (i, i, i, i), (j, j, j, j))]
symmetrizeAreaIndices2 = concat . map (\(r, a, b) -> [(r / 2, a, b), (-r / 2, swap2 a, b)])

symmetrizeAreaIndices3 :: Fractional a => [(a, (i, i, i, i), (j, j, j, j))] -> [(a, (i, i, i, i), (j, j, j, j))]
symmetrizeAreaIndices3 = concat . map (\(r, a, b) -> [(r / 2, a, b), (r / 2, swap3 a, b)])

symmetrizeAreaIndices4 :: Fractional a => [(a, (i, i, i, i), (j, j, j, j))] -> [(a, (i, i, i, i), (j, j, j, j))]
symmetrizeAreaIndices4 = concat . map (\(r, a, b) -> [(r / 2, a, b), (-r / 2, a, swap1 b)])

symmetrizeAreaIndices5 :: Fractional a => [(a, (i, i, i, i), (j, j, j, j))] -> [(a, (i, i, i, i), (j, j, j, j))]
symmetrizeAreaIndices5 = concat . map (\(r, a, b) -> [(r / 2, a, b), (-r / 2, a, swap2 b)])

symmetrizeAreaIndices6 :: Fractional a => [(a, (i, i, i, i), (j, j, j, j))] -> [(a, (i, i, i, i), (j, j, j, j))]
symmetrizeAreaIndices6 = concat . map (\(r, a, b) -> [(r / 2, a, b), (r / 2, a, swap3 b)])

{- apply all symmetrizations -}
symmetrizeAreaIndices :: Fractional a => (i, i, i, i) -> (j, j, j, j) -> [(a, (i, i, i, i), (j, j, j, j))]
symmetrizeAreaIndices a b =( symmetrizeAreaIndices1 .
                                                     symmetrizeAreaIndices2 .
                                                     symmetrizeAreaIndices3 .
                                                     symmetrizeAreaIndices4 .
                                                     symmetrizeAreaIndices5 .
                                                     symmetrizeAreaIndices6) [(1, a, b)]

{- calculate value of spacetime Gotay-Marsden intertwiners
 - fold and inner function -}
innerIntertwiner :: Fractional a => ISUp -> ISDown -> a -> (a, AreaIndicesDown, AreaIndicesUp) -> a
innerIntertwiner m n r1 (r2, a, b) = r1 + r2 * spacetimeDelta b a m n

spacetimeIntertwiner :: Fractional a => AreaIndicesDown -> AreaIndicesUp -> ISUp -> ISDown -> a
spacetimeIntertwiner a b m n = let symAreas = symmetrizeAreaIndices a b
                                in foldl (innerIntertwiner m n) 0 symAreas

{- calculate value of Gotay-Marsden intertwiner using dof indices -}
areaIntertwiner :: Fractional a => Map.Map Int AreaIndices -> IGDown -> IGUp -> ISUp -> ISDown -> a
areaIntertwiner indicesMap a b m n = intertwinerFactor (aIUpfromAI (indicesMap Map.! (unIGUp b))) *
                                      spacetimeIntertwiner (aIDownfromAI (indicesMap Map.! (unIGDown a)))
                                                           (aIUpfromAI (indicesMap Map.! (unIGUp b))) m n

buildAreaIntertwinerMap :: Fractional a => Map.Map (IGDown, IGUp, ISUp, ISDown) a
buildAreaIntertwinerMap = Map.fromList $ let indicesMap = buildAreaIndicesMap in
                           do a <- areaRangeDown
                              b <- areaRangeUp
                              m <- spacetimeRangeUp
                              n <- spacetimeRangeDown
                              return ((a, b, m, n), areaIntertwiner indicesMap a b m n)
