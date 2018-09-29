module IntertwinerSym3 where

import qualified Data.Map.Strict as Map
import Index

type Sym3Indices     = (Int, Int, Int)
type Sym3IndicesUp   = (ISUp, ISUp, ISUp)
type Sym3IndicesDown = (ISDown, ISDown, ISDown)

s3IUpfromS3I :: Sym3Indices -> Sym3IndicesUp
s3IUpfromS3I (a, b, c) = (ISUp a, ISUp b, ISUp c)

s3IDownfromS3I :: Sym3Indices -> Sym3IndicesDown
s3IDownfromS3I (a, b, c) = (ISDown a, ISDown b, ISDown c)

{- dimension of spacetime -}
spacetimeDimension :: Int
spacetimeDimension = 4

{- Range of sym3 variables -}
sym3Range :: [Int]
sym3Range = [0..((spacetimeDimension * (spacetimeDimension + 1) * (spacetimeDimension + 2)) `div` (1*2*3)) - 1]

sym3RangeUp :: [IS3Up]
sym3RangeUp = map IS3Up sym3Range

sym3RangeDown :: [IS3Down]
sym3RangeDown = map IS3Down sym3Range

{- Range of spacetime dimensions -}
spacetimeRange :: [Int]
spacetimeRange = [0..spacetimeDimension - 1]

spacetimeRangeUp :: [ISUp]
spacetimeRangeUp = map ISUp spacetimeRange

spacetimeRangeDown :: [ISDown]
spacetimeRangeDown = map ISDown spacetimeRange

{- distribution of sym3 dof in sym3 spacetime tensor -}
buildSym3IndicesMap :: Map.Map Int Sym3Indices
buildSym3IndicesMap = Map.fromList $ zip 
                       [0..]
                       [ (0, 0, 0),
                         (0, 0, 1),
                         (0, 0, 2),
                         (0, 0, 3),
                         (0, 1, 1),
                         (0, 1, 2),
                         (0, 1, 3),
                         (0, 2, 2),
                         (0, 2, 3),
                         (0, 3, 3),
                         (1, 1, 1),
                         (1, 1, 2),
                         (1, 1, 3),
                         (1, 2, 2),
                         (1, 2, 3),
                         (1, 3, 3),
                         (2, 2, 2),
                         (2, 2, 3),
                         (2, 3, 3),
                         (3, 3, 3) ]

{- factor in front of intertwiners
 - dependent on distribution of dof above -}
intertwinerFactor :: Num a => Sym3IndicesUp -> a
intertwinerFactor (a, b, c)
        | a == b = if a == c then 1 else 3
        | a == c = if a == b then 1 else 3
        | b == c = if a == b then 1 else 3
        | otherwise = 6

dofIntertwiner :: Fractional a => Sym3Indices -> Int -> Int -> Int -> a
dofIntertwiner (a, b, c) m n p = let summand1 = if (a == m) && (b == n) && (c == p) then (1 / 6) else 0
                                     summand2 = if (a == m) && (b == p) && (c == n) then (1 / 6) else 0
                                     summand3 = if (a == n) && (b == m) && (c == p) then (1 / 6) else 0
                                     summand4 = if (a == n) && (b == p) && (c == m) then (1 / 6) else 0
                                     summand5 = if (a == p) && (b == m) && (c == n) then (1 / 6) else 0
                                     summand6 = if (a == p) && (b == n) && (c == m) then (1 / 6) else 0
                                 in summand1 + summand2 + summand3 + summand4 + summand5 + summand6

{- calculate value of dof-to-spacetime intertwiner -}
sym3Intertwiner :: Fractional a => Map.Map Int Sym3Indices -> IS3Up -> (ISDown, ISDown, ISDown) -> a
sym3Intertwiner indicesMap (IS3Up a) (ISDown m, ISDown n, ISDown p) = intertwinerFactor (s3IUpfromS3I (indicesMap Map.! a)) *
                                                                     dofIntertwiner (indicesMap Map.! a) m n p

buildSym3IntertwinerMap :: Fractional a => Map.Map (IS3Up, (ISDown, ISDown, ISDown)) a
buildSym3IntertwinerMap = Map.fromList $ let indicesMap = buildSym3IndicesMap in
                           do a <- sym3RangeUp
                              m <- spacetimeRangeDown
                              n <- spacetimeRangeDown
                              p <- spacetimeRangeDown
                              return ((a, (m, n, p)), sym3Intertwiner indicesMap a (m, n, p))

{- calculate value of spacetime-to-dof projector -}
sym3Projector :: Fractional a => Map.Map Int Sym3Indices -> (ISUp, ISUp, ISUp) -> IS3Down -> a
sym3Projector indicesMap (ISUp m, ISUp n, ISUp p) (IS3Down a) = dofIntertwiner (indicesMap Map.! a) m n p

buildSym3ProjectorMap :: Fractional a => Map.Map ((ISUp, ISUp, ISUp), IS3Down) a
buildSym3ProjectorMap = Map.fromList $ let indicesMap = buildSym3IndicesMap in
                                        do a <- sym3RangeDown
                                           m <- spacetimeRangeUp
                                           n <- spacetimeRangeUp
                                           p <- spacetimeRangeUp
                                           return (((m, n, p), a), sym3Projector indicesMap (m, n, p) a)

