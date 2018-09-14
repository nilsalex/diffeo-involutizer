module Main where

import PDE
import DiffeoEquationsArea
import Data.List
import Data.List.Ordered
import Data.Ratio
import System.Random

main :: IO ()
main = do
        let pde = areaPDE1
        let pdeProlonged = prolongAreaPDESystem pde
--        putStr $ showPDESys pdeProlonged
        gen <- getStdGen
--        putStr $ showPDESys $ evalPDESystemRand gen 315 pdeProlonged
        let randMat = randPDESysToMat $ evalPDESystemRand gen 315 pdeProlonged
--        print $ length pdeProlonged
--        print $ length $ filter (\((_, dep), _) -> dep > 314) randMat
--        print randMat
        putStr $ showMat randMat

showMat :: PDEMat -> String
showMat = unlines . (map showEntry)

showEntry :: PDEMatEntry -> String
showEntry ((i, j), r) = show i ++ "," ++ show j ++ "," ++ show (numerator r)

showPDESys :: Show a => [[a]] -> String
showPDESys = unlines . (map showPDE)

showPDE :: Show a => [a] -> String
showPDE = unlines . (map show)
