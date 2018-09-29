module DiffeoInvolutizer (dimain) where

import PDE
import PDEProlongation
import DiffeoEquationsMetric

import System.Random

getPDE :: PDESystem Rational
getPDE = metricPDE

dimain :: IO ()
dimain = do
          gen <- getStdGen
          let pde = getPDE
          let prolonged = systemProlongations pde
          let symbol = pdeSystemSymbol prolonged
          let eval = evalPDESystemRand gen symbol
          putStrLn $ prettyPDEMatrixSecond eval
{-          
          let eval = evalPDESystemRand gen symbol
          print $ pdeSystemMinOrder eval
          print $ pdeSystemOrder eval
          putStr $ prettyPDEMatrixSecond eval
          -}
