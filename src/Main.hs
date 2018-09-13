module Main where

import DiffeoEquationsArea
import Data.Ratio

main :: IO ()
main = do
        let pde = areaPDE1Part1
        putStrLn $ showPDESys pde

showPDESys :: [PDE] -> String
showPDESys = unlines . (map showPDE)

showPDE :: PDE -> String
showPDE = unlines . (map show)
