module PDE where

data Coefficient = Null | Constant Rational | Linear Rational Integer deriving (Show, Eq)
data Term = Term { coefficient :: Coefficient, independent :: Integer } deriving (Show, Eq)

type PDE = [Term]
