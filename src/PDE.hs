module PDE where

import Control.Monad.Random
import Data.List
import Data.List.Ordered
import qualified Data.Map as Map
import Data.Ratio

data Coefficient = Null | Constant Rational | Linear Rational Integer deriving (Show, Eq)
data Term = Term { coefficient :: Coefficient, dependent :: Integer } deriving (Show, Eq)
data TermRand = TermRand { coefficientR :: Rational, dependentR :: Integer } deriving (Show, Eq)

type PDE = [Term]
type PDERand = [TermRand]

type PDEMatEntry = ((Integer, Integer), Rational)
type PDEMat = [PDEMatEntry]

buildRandomIdepsMap :: StdGen -> Integer -> Map.Map Integer Rational
buildRandomIdepsMap gen ideps = Map.fromList $
                             zip [0..ideps-1] $ map (fromIntegral :: Integer -> Rational) (randoms gen)

evalTermRand :: Map.Map Integer Rational -> Term -> TermRand
evalTermRand randMap (Term {coefficient = coeff, dependent = dep})
        = case coeff of Null          -> TermRand { coefficientR = 0, dependentR = dep }
                        Constant r    -> TermRand { coefficientR = r, dependentR = dep }
                        Linear r idep -> TermRand { coefficientR = r * randMap Map.! idep, dependentR = dep }

evalPDERand :: Map.Map Integer Rational -> PDE -> PDERand
evalPDERand randMap = (filter (\TermRand { coefficientR = coeff } -> coeff /= 0)) . (map (evalTermRand randMap))

evalPDESystemRand :: StdGen -> Integer -> [PDE] -> [PDERand]
evalPDESystemRand gen ideps = map (evalPDERand (buildRandomIdepsMap gen ideps))

randTermToMatEntry :: Integer -> TermRand -> PDEMatEntry
randTermToMatEntry eq (TermRand { coefficientR = r, dependentR = dep }) = ((eq, dep), r)

concatMatEntries :: PDEMat -> PDEMatEntry
concatMatEntries ((p0,r0):xs) = foldl (\(p, r) (p', r') -> (p0, r + r')) (p0, r0) xs
concatMatEntries [] = undefined

randPDEToMat :: Integer -> PDERand -> PDEMat
randPDEToMat i = (filter (\(_, r) -> r /= 0))
                    . (map concatMatEntries)
                    . (groupBy (\(pair1, _) (pair2, _) -> pair1 == pair2))
                    . sort
                    . (map (randTermToMatEntry i))

randPDESysToMat :: [PDERand] -> PDEMat
randPDESysToMat = concat . (map (\(i, pde) -> randPDEToMat i pde)) . (zip [0..])
