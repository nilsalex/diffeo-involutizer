module Polynomial where

import MultiIndex

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

data Polynomial a = Polynomial Int (M.Map MultiIndex a) deriving (Show, Eq, Ord)

emptyPolynomial :: Int -> Polynomial a
emptyPolynomial len = Polynomial len M.empty

quartic :: (Num a, Eq a) => Int -> Int -> Int -> Int -> Int -> a -> Polynomial a
quartic _ _ _ _ _ 0       = undefined
quartic len a b c d coeff = Polynomial len (M.singleton (fromList len [a, b, c, d]) coeff)

monomial :: (Num a, Eq a) => Int -> [Int] -> a -> Polynomial a
monomial _ _ 0        = undefined
monomial len is coeff = Polynomial len (M.singleton (fromList len is) coeff)

addPolynomials :: (Num a, Eq a) => Polynomial a -> Polynomial a -> Polynomial a
addPolynomials (Polynomial len pmap) (Polynomial len' pmap')
        | len /= len' = undefined
        | len <= 0    = undefined
        | otherwise   = Polynomial len $ M.filter (\c -> c /= 0) $ M.unionWith (\c c' -> c + c') pmap pmap'

prettyMonomial :: MultiIndex -> String
prettyMonomial (MIx _ _ imap) = drop 3 $ IM.foldMapWithKey (\v p -> " * x_" ++ show v ++ if p > 1 then "^" ++ show p
                                                                                                  else "") imap

prettyMonomialMetric :: MultiIndex -> String
prettyMonomialMetric (MIx _ _ imap) = drop 3 $ IM.foldMapWithKey (\v p -> (if v < 150 then " * x_" ++ show (v + 1)
                                                                                      else " * y_" ++ show (v + 1 - 150))
                                                                          ++
                                                                          (if p > 1 then "^" ++ show p
                                                                                    else ""))
                                                                 imap

prettyPolynomial :: Show a => Polynomial a -> String
prettyPolynomial (Polynomial len pmap) = M.foldMapWithKey
                                         (\mIx coeff -> "+ (" ++ show coeff ++ ") * " ++ prettyMonomial mIx ++ "\n")
                                         pmap

prettyPolynomialMetric :: Show a => Polynomial a -> String
prettyPolynomialMetric (Polynomial len pmap) = M.foldMapWithKey
                                               (\mIx coeff -> "+ (" ++ show coeff ++ ") * " ++ prettyMonomialMetric mIx ++ "\n")
                                               pmap
