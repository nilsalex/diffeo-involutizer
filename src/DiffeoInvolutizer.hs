module DiffeoInvolutizer (dimain) where

import PDE
import PDEProlongation
--import DiffeoEquationsMetric
import DiffeoEquationsArea

import System.Random.TF

getPDE :: PDESystem Rational
--getPDE = metricPDE
getPDE = areaPDE

dimain :: IO ()
dimain = do
          let pde = getPDE
          let pro = systemProlongations pde
          let total = concatPDESystems pde pro
          let sym = pdeSystemSymbol pro
          let eval = evalAreaAtEta total
          putStr $ prettyPDEMatrix eval
