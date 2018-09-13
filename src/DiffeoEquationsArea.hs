module DiffeoEquationsArea where

import qualified IntertwinerArea
import qualified IntertwinerMetric
import qualified Data.Map.Strict as Map
import Data.Ratio

data Coefficient = Null | Constant Rational | Linear [(Coefficient, Integer)] deriving (Show, Eq)
data Term = Term { coefficient :: Coefficient, independent :: Integer } deriving (Show, Eq)

type PDE = [Term]

spacetimeRange :: [Integer]
spacetimeRange = [0..3]

areaRange :: [Integer]
areaRange = [0..20]

delta :: Integer -> Integer -> Rational
delta m n = if m == n then 1 % 1 else 0 % 1

areaPDE1Part1 :: [PDE]
areaPDE1Part1 = let areaIntertwinerMap = IntertwinerArea.buildAreaIntertwinerMap in
                do m <- spacetimeRange
                   n <- spacetimeRange
                   let pde = areaPDE1Part1Inner areaIntertwinerMap m n
                   let pdeReduced = filter (\Term { coefficient = c } -> c /= Null) pde
                   return pdeReduced

areaPDE1Part1Inner :: Map.Map (Integer, Integer, Integer, Integer) Rational -> Integer -> Integer -> PDE
areaPDE1Part1Inner areaIntMap m n = do a <- areaRange
                                       b <- areaRange
                                       let prefactor = areaIntMap Map.! (a, b, m, n)
                                       return $ Term { coefficient = if prefactor == 0 then Null else Linear [(Constant prefactor, b)]
                                                       , independent = a }
