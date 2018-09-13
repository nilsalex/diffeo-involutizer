module Main where

import PDE
import DiffeoEquationsArea
import Data.Ratio

main :: IO ()
main = do
        let pde = areaPDE1
        let pdeProlonged = prolongAreaPDESystem pde
        print $ length pdeProlonged
        putStr $ showPDESys pdeProlonged

showPDESys :: [PDE] -> String
showPDESys = unlines . (map showPDE)

showPDE :: PDE -> String
showPDE = unlines . (map show)
