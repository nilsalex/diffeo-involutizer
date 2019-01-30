module Dof
    ( dofMain
    ) where

import Data.Ratio
import qualified Data.Map as M
import Data.List

triMap :: Int -> M.Map (Int, Int) Int
triMap l = M.fromList triList
    where
        pairs = concat $ map (\i -> zip (repeat i) [i..l]) [1..l]
        triList = zip pairs [1..]

triMap3 :: Int -> M.Map (Int, Int, Int) Int
triMap3 l = M.fromList triList
    where
        tuples = [(a, b, c) | a <- [1..l], b <- [a..l], c <- [b..l]]
        triList = zip tuples [1..]

s2Map :: M.Map (Int, Int) Int
s2Map = M.fromList $ [
            ((0, 0), 1),
            ((0, 1), 2),
            ((0, 2), 3),
            ((0, 3), 4),
            ((1, 1), 5),
            ((1, 2), 6),
            ((1, 3), 7),
            ((2, 2), 8),
            ((2, 3), 9),
            ((3, 3), 10)
        ]

areaMap :: M.Map (Int, Int, Int, Int) Int
areaMap = M.fromList $ [
            ((0, 1, 0, 1), 1),
            ((0, 1, 0, 2), 2),
            ((0, 1, 0, 3), 3),
            ((0, 1, 1, 2), 4),
            ((0, 1, 1, 3), 5),
            ((0, 1, 2, 3), 6),
            ((0, 2, 0, 2), 7),
            ((0, 2, 0, 3), 8),
            ((0, 2, 1, 2), 9),
            ((0, 2, 1, 3), 10),
            ((0, 2, 2, 3), 11),
            ((0, 3, 0, 3), 12),
            ((0, 3, 1, 2), 13),
            ((0, 3, 1, 3), 14),
            ((0, 3, 2, 3), 15),
            ((1, 2, 1, 2), 16),
            ((1, 2, 1, 3), 17),
            ((1, 2, 2, 3), 18),
            ((1, 3, 1, 3), 19),
            ((1, 3, 2, 3), 20),
            ((2, 3, 2, 3), 21)
          ]

areaNum :: M.Map (Int, Int, Int, Int) Int ->
           Int -> Int -> Int -> Int -> Int
areaNum am a b c d = am M.! (a, b, c, d)

areaSNum :: M.Map (Int, Int, Int, Int) Int ->
            Int -> Int -> Int -> Int -> Int -> Int
areaSNum am a b c d p = 21 + 4 * (aNum - 1) + p + 1
    where
        aNum = am M.! (a, b, c, d)

areaS2Num :: M.Map (Int, Int, Int, Int) Int -> M.Map (Int, Int) Int ->
             Int -> Int -> Int -> Int -> Int -> Int -> Int
areaS2Num am s2m a b c d p q = 21 + 84 + 10 * (aNum - 1) + sNum
    where
        aNum = am M.! (a, b, c, d)
        sNum = s2m M.! (p, q)

idepNum :: M.Map (Int, Int) Int -> M.Map (Int, Int, Int) Int ->
           M.Map (Int, Int) Int -> M.Map (Int, Int, Int, Int) Int -> [[Int]] -> Int
idepNum tm tm3 s2m am is = case is of
    []                                           -> 1
    [[a, b, c, d]]                               -> 1 + areaNum am a b c d
    [[a, b, c, d], [p, q]]                       -> 1 + areaS2Num am s2m a b c d p q
    [[a, b, c, d], [e, f, g, h]]                 -> 1 + 315 + (tm M.! (areaNum am a b c d, areaNum am e f g h))
    [[a, b, c, d], [e, f, g, h], [p, q]]         -> 1 + 315 + (tm M.! (areaNum am a b c d, areaS2Num am s2m e f g h p q))
    [[a, b, c, d], [e, f, g, h], [p], [q]]       -> 1 + 315 + (tm M.! (areaSNum am a b c d p, areaSNum am e f g h q))
    [[a, b, c, d], [e, f, g, h], [p, q], [r, s]] -> 1 + 315 + (tm M.! (areaS2Num am s2m a b c d p q, areaS2Num am s2m e f g h r s))
    [[a, b, c, d], [e, f, g, h], [i, j, k, l]]   -> 1 + 315 + ((315*316) `div` 2) +
                                                    (tm3 M.! (areaNum am a b c d, areaNum am e f g h, areaNum am i j k l))
    [[a, b, c, d], [e, f, g, h], [i, j, k, l], [p], [q]]
        -> 1 + 315 + ((315*316) `div` 2) + (tm3 M.! (areaNum am a b c d, areaSNum am e f g h p, areaSNum am i j k l q))
    [[a, b, c, d], [e, f, g, h], [i, j, k, l], [p, q], [r, s]]
        -> 1 + 315 + ((315*316) `div` 2) + (tm3 M.! (areaNum am a b c d, areaS2Num am s2m e f g h p q, areaS2Num am s2m i j k l r s))
    [[a, b, c, d], [e, f, g, h], [i, j, k, l], [p], [q], [r, s]]
        -> 1 + 315 + ((315*316) `div` 2) + (tm3 M.! (areaSNum am a b c d p, areaSNum am e f g h q, areaS2Num am s2m i j k l r s))

areaFactor :: Fractional a => Int -> Int -> Int -> Int -> a
areaFactor a b c d
    | a == c && b == d = 4
    | otherwise = 8

s2Factor :: Fractional a => Int -> Int -> a
s2Factor p q
    | p == q = 1
    | otherwise = 2

factor :: Fractional a => [[Int]] -> a
factor [] = 1
factor [[a, b, c, d]] = areaFactor a b c d
factor [[a, b, c, d], [p]] = areaFactor a b c d
factor [[a, b, c, d], [p, q]] = areaFactor a b c d * s2Factor p q
factor [[a, b, c, d], [e, f, g, h]] = areaFactor a b c d * areaFactor e f g h
factor [[a, b, c, d], [e, f, g, h], [p, q]] = areaFactor a b c d * areaFactor e f g h * s2Factor p q
factor [[a, b, c, d], [e, f, g, h], [p], [q]] = areaFactor a b c d * areaFactor e f g h
factor [[a, b, c, d], [e, f, g, h], [p, q], [r, s]] = areaFactor a b c d * areaFactor e f g h * s2Factor p q * s2Factor r s
factor [[a, b, c, d], [e, f, g, h], [i, j, k, l]] = areaFactor a b c d * areaFactor e f g h * areaFactor i j k l
factor [[a, b, c, d], [e, f, g, h], [i, j, k, l], [p], [q]] = areaFactor a b c d * areaFactor e f g h * areaFactor i j k l
factor [[a, b, c, d], [e, f, g, h], [i, j, k, l], [p, q], [r, s]] = areaFactor a b c d * areaFactor e f g h * areaFactor i j k l
                                                                     * s2Factor p q * s2Factor r s
factor [[a, b, c, d], [e, f, g, h], [i, j, k, l], [p], [q], [r, s]] = areaFactor a b c d * areaFactor e f g h * areaFactor i j k l
                                                                       * s2Factor r s

toMatrix :: M.Map (Int, Int) Int -> M.Map (Int, Int, Int) Int ->
            M.Map (Int, Int) Int -> M.Map (Int, Int, Int, Int) Int ->
            ([[Int]], [(Int, Rational)]) -> [(Int, Int, Rational)]
toMatrix tm tm3 s2m am (is, vs) = ys
    where
        i = idepNum tm tm3 s2m am is
        ys = map (\(j, v) -> (i, j, v * factor is)) vs

dofMain :: IO ()
dofMain = do
            str <- readFile "dof.txt"
            let tm = triMap 315
            let tm3 = triMap3 315
            let am = areaMap
            let s2m = s2Map
            let dof = map read $ lines str :: [([[Int]], [(Int, Rational)])]
            let mat = concat $ map (toMatrix tm tm3 s2m am) dof
--            putStr $ unlines $ map show $ nub $ sort $ map (\(_, _, v) -> denominator v) mat
            writeFile "dof.dat" $ unlines $ map (\(i, j, v) -> let v' = 12 * v in if denominator v' /= 1 then undefined else show i ++ " " ++ show j ++ " " ++ show (numerator v')) mat
--            putStr $ unlines $ map (\(i, j, v) -> let v' = 6 * v in if denominator v' /= 1 then undefined else show i ++ " " ++ show j ++ " " ++ show (numerator v')) mat
