module DiffeoInvolutizer (dimain) where

import PDE
import PDEProlongation
import DiffeoEquationsArea

import IntertwinerMetric
import Polynomial

import System.Random

getPDE :: PDESystem Rational
getPDE = areaPDE

dimain :: IO ()
dimain = do
{-
          gen <- getStdGen
          let pde = getPDE
          let pro = systemProlongations pde
          putStr $ prettyPDESystem pro
-}
          let ricci = addPolynomials ricci1 ricci2 :: Polynomial Rational
          putStr $ prettyPolynomialMetric ricci
