module Tests (tmain) where

import PDE
import PDEProlongationTest

tmain :: IO ()
tmain = do
         putStrLn "original system :"
         putStr $ prettyPDESystem testDataPDE1
         putStrLn "prolongations :"
         putStr $ prettyPDESystem testDataPDE1Prolonged
         putStrLn $ "original symbol :"
         putStr $ prettyPDESystem $ pdeSystemSymbol testDataPDE1
         putStrLn $ "prolonged symbol :"
         putStr $ prettyPDESystem $ pdeSystemSymbol testDataPDE1Prolonged
