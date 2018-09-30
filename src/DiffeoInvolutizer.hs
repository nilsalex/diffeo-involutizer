module DiffeoInvolutizer (dimain) where

import Polynomial
import IntertwinerMetric

{-
getPDE :: PDESystem Rational
getPDE = metricPDE
-}

dimain :: IO ()
dimain = putStr $ prettyPolynomial (metricDeterminant :: Polynomial Rational)
{-
          gen <- getStdGen
          let pde = getPDE
          let prolonged = systemProlongations pde
          let symbol = pdeSystemSymbol prolonged
          let eval = evalPDESystemRand gen symbol
-}
{-          
          putStrLn $ prettyPDEMatrixSecond eval
          let eval = evalPDESystemRand gen symbol
          print $ pdeSystemMinOrder eval
          print $ pdeSystemOrder eval
          putStr $ prettyPDEMatrixSecond eval
          -}
