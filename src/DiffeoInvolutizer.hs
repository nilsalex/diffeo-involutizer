module DiffeoInvolutizer (dimain) where

import PDE
import PDEProlongation
--import DiffeoEquationsMetric
import DiffeoEquationsArea

import System.Random.TF

getPDE :: PDESystem Rational
--getPDE = metricPDE
getPDE = areaPDE

getExInt :: PDESystem Rational
getExInt = areaExInt3

dimain :: IO ()
dimain = do
          let pde = getPDE
          let ex = getExInt
          let pro = systemProlongations pde
          let pro2 = systemProlongations pro
          let total = concatPDESystems pde pro
          let sym = pdeSystemSymbol pro
          let eval = evalAreaAtEta total
          putStr $ prettyPDEMatrix eval
