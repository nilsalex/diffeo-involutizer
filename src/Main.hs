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
        let pdeProlonged = pde ++ (prolongAreaPDESystem pde)
        gen <- getStdGen
        let randMat = randPDESysToMat $ evalPDESystemRand gen 315 pdeProlonged
        let randMatSym = map (\((eq, dep), r) -> ((eq+1, dep+1), r)) {-$ filter (\((_, dep), _) -> dep > 314)-} randMat
        putStr $ showMat randMatSym

showMat :: PDEMat -> String
showMat = unlines . (map showEntry)

showEntry :: PDEMatEntry -> String
--showEntry ((i, j), r) = show i ++ "," ++ show j ++ "," ++ show (numerator r)
showEntry ((i, j), r) = "(" ++ show i ++ "," ++ show j ++ ") = " ++ show (numerator r) ++ ","

showPDESys :: Show a => [[a]] -> String
showPDESys = unlines . (map showPDE)

showPDE :: Show a => [a] -> String
showPDE = unlines . (map show)
