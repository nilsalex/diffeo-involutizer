module DiffeoInvolutizer (dimain) where

import PDE
import PDEProlongation
import DiffeoEquationsArea

import System.Random.TF

getPDE :: PDESystem Rational
getPDE = areaPDE

dimain :: IO ()
dimain = do
          let pde = getPDE
          let pro = systemProlongations pde
          let full = concatPDESystems pde pro
          gen <- newTFGen
          let eval = evalPDESystemRand gen full
          putStr $ prettyPDEMatrix eval
