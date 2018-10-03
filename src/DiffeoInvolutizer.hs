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
          let pde = getPDE
          putStr $ prettyPDESystem pde
