module IntertwinerSym3 where

import qualified Data.Map.Strict as Map
import Data.Ratio

type Sym3Indices = (Integer, Integer, Integer)

{- dimensino of spacetime -}
spacetimeDimension :: Integer
spacetimeDimension = 4

{- Range of sym3 variables -}
sym3Range :: [Integer]
sym3Range = [0..((spacetimeDimension * (spacetimeDimension + 1) * (spacetimeDimension + 2)) `div` (1*2*3)) - 1]

{- Range of spacetime dimensions -}
spacetimeRange :: [Integer]
spacetimeRange = [0..spacetimeDimension - 1]

{- distribution of sym3 dof in sym3 spacetime tensor -}
buildSym3IndicesMap :: Map.Map Integer Sym3Indices
buildSym3IndicesMap = Map.fromList $ zip 
                       [0..]
                       [ (0, 0, 0),
                         (1, 1, 1),
                         (2, 2, 2),
                         (3, 3, 3),
                         (0, 0, 1),
                         (0, 0, 2),
                         (0, 0, 3),
                         (0, 1, 1),
                         (0, 2, 2),
                         (0, 3, 3),
                         (1, 1, 2),
                         (1, 1, 3),
                         (1, 2, 2),
                         (1, 3, 3),
                         (2, 2, 3),
                         (2, 3, 3),
                         (0, 1, 2),
                         (0, 1, 3),
                         (0, 2, 3),
                         (1, 2, 3)  ]

{- factor in front of intertwiners
 - dependent on distribution of dof above -}
intertwinerFactor :: Integer -> Rational
intertwinerFactor a
    | a < 4 = 1
    | a < 16 = 3
    | otherwise = 6

dofIntertwiner :: Sym3Indices -> Integer -> Integer -> Integer -> Rational
dofIntertwiner (a, b, c) m n p = let summand1 = if (a == m) && (b == n) && (c == p) then (1 % 6) else (0 % 1)
                                     summand2 = if (a == m) && (b == p) && (c == n) then (1 % 6) else (0 % 1)
                                     summand3 = if (a == n) && (b == m) && (c == p) then (1 % 6) else (0 % 1)
                                     summand4 = if (a == n) && (b == p) && (c == m) then (1 % 6) else (0 % 1)
                                     summand5 = if (a == p) && (b == m) && (c == n) then (1 % 6) else (0 % 1)
                                     summand6 = if (a == p) && (b == n) && (c == m) then (1 % 6) else (0 % 1)
                                 in summand1 + summand2 + summand3 + summand4 + summand5 + summand6

{- calculate value of dof-to-spacetime intertwiner -}
sym3Intertwiner :: Map.Map Integer Sym3Indices -> Integer -> (Integer, Integer, Integer) -> Rational
sym3Intertwiner indicesMap a (m, n, p) = intertwinerFactor a * dofIntertwiner (indicesMap Map.! a) m n p

buildSym3IntertwinerMap :: Map.Map (Integer, (Integer, Integer, Integer)) Rational
buildSym3IntertwinerMap = Map.fromList $ let indicesMap = buildSym3IndicesMap in
                           do a <- sym3Range
                              m <- spacetimeRange
                              n <- spacetimeRange
                              p <- spacetimeRange
                              return ((a, (m, n, p)), sym3Intertwiner indicesMap a (m, n, p))

{- calculate value of spacetime-to-dof projector -}
sym3Projector :: Map.Map Integer Sym3Indices -> (Integer, Integer, Integer) -> Integer -> Rational
sym3Projector indicesMap (m, n, p) a = dofIntertwiner (indicesMap Map.! a) m n p

buildSym3ProjectorMap :: Map.Map ((Integer, Integer, Integer), Integer) Rational
buildSym3ProjectorMap = Map.fromList $ let indicesMap = buildSym3IndicesMap in
                                        do a <- sym3Range
                                           m <- spacetimeRange
                                           n <- spacetimeRange
                                           p <- spacetimeRange
                                           return (((m, n, p), a), sym3Projector indicesMap (m, n, p) a)

