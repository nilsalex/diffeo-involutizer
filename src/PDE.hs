{-# LANGUAGE TemplateHaskell #-}

module PDE where

import MultiIndex
import Control.Monad.Random
import Test.QuickCheck
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import Data.List

data Num a => Coefficient a = Null | Constant a | Affine a (IM.IntMap a) deriving (Show, Eq, Ord)

data Num a => PDE a = PDECons (M.Map MultiIndex (Coefficient a)) deriving (Show, Eq, Ord)

data Num a => PDESystem a = PDESys (S.Seq (PDE a)) deriving (Show, Eq, Ord)

coeffFromConst :: (Eq a, Num a) => a -> Coefficient a
coeffFromConst 0 = Null
coeffFromConst c = Constant c

coeffFromMap :: (Num a, Eq a) => IM.IntMap a -> Coefficient a
coeffFromMap imap = if nonZero then Affine 0 nonZeroMap else Null
                    where nonZeroMap = IM.filter ((/=) 0) imap
                          nonZero = not (IM.null nonZeroMap)

coeffFromConstMap :: (Num a, Eq a) => a -> IM.IntMap a -> Coefficient a
coeffFromConstMap c imap = if nonZero then Affine c nonZeroMap else coeffFromConst c
                           where nonZeroMap = IM.filter ((/=) 0) imap
                                 nonZero = not (IM.null nonZeroMap)

coeffFromConstIdep :: (Eq a, Num a) => a -> Int -> Coefficient a
coeffFromConstIdep 0 i = Null
coeffFromConstIdep c i = coeffFromMap $ IM.singleton i c

getCoeffMap :: Num a => Coefficient a -> Either a (a, IM.IntMap a)
getCoeffMap (Null)           = Left 0
getCoeffMap (Constant a)     = Left a
getCoeffMap (Affine a cmap)  = Right (a, cmap)

isZero :: (Num a, Eq a) => Coefficient a -> Bool
isZero (Null)          = True
isZero (Constant c  )  = c == 0
isZero (Affine c imap) = c == 0 && all ((==) 0) imap

addCoefficients :: (Num a, Eq a) => Coefficient a -> Coefficient a -> Coefficient a
addCoefficients (Null) coeff = coeff
addCoefficients coeff (Null) = coeff
addCoefficients (Constant a) (Constant b) = coeffFromConst (a+b)
addCoefficients (Constant a) (Affine b imap) = Affine (a+b) imap
addCoefficients (Affine a imap) (Constant b) = Affine (a+b) imap
addCoefficients (Affine a imap) (Affine b imap') = coeffFromConstMap (a+b) $ IM.unionWith (+) imap imap'

coeffDerivative :: (Num a, Eq a) => Int -> Coefficient a -> Coefficient a
coeffDerivative _ Null = Null
coeffDerivative _ (Constant _) = Null
coeffDerivative derivative (Affine _ imap) = case IM.lookup derivative imap of Nothing  -> Null
                                                                               (Just c) -> coeffFromConst c

prettyCoefficient :: (Show a, Num a, Eq a, Ord a) => Coefficient a -> String
prettyCoefficient Null = "0"
prettyCoefficient (Constant c) = show c
prettyCoefficient (Affine 0 imap) = let parentheses = IM.size imap > 1 in
                                        (if parentheses then "(" else "") ++
                                            (prettyCoefficientLinear 3 imap) ++
                                            (if parentheses then ")" else "")
prettyCoefficient (Affine c imap) = "(" ++ show c ++ (prettyCoefficientLinear 0 imap) ++ ")"


prettyCoefficientLinear :: (Show a, Num a, Ord a) => Int -> IM.IntMap a -> String
prettyCoefficientLinear d = drop d . IM.foldMapWithKey (\i c -> (if signum c < 0 then " - " else " + ") ++ show (abs c))

emptyPDE :: Num a => PDE a
emptyPDE = PDECons M.empty

pdeFromMap :: (Num a, Eq a) => M.Map MultiIndex (Coefficient a) -> PDE a
pdeFromMap = PDECons . (M.filter (not . isZero))

pdeFromConstIdepDerivative :: (Num a, Eq a) => Int -> a -> Int -> Int -> PDE a
pdeFromConstIdepDerivative _ 0 _ _ = pdeFromMap $ M.empty
pdeFromConstIdepDerivative len c i d = pdeFromMap $ M.singleton (single len d) (coeffFromConstIdep c i)

getPDEMap :: Num a => PDE a -> M.Map MultiIndex (Coefficient a)
getPDEMap (PDECons pdeMap) = pdeMap

addPDEs :: (Num a, Eq a) => PDE a -> PDE a -> PDE a
addPDEs (PDECons pdeMapA) (PDECons pdeMapB) = pdeFromMap $ M.unionWith addCoefficients pdeMapA pdeMapB

buildRandomIdepsMap :: (Num a, Random a, RandomGen r) => r -> Int -> M.Map Int a 
buildRandomIdepsMap gen ideps = M.fromList $ zip [0..ideps-1] $ randoms gen

evalCoefficientRand :: (Num a, Eq a, Random a) => M.Map Int a -> Coefficient a -> Coefficient a
evalCoefficientRand _ Null = Null
evalCoefficientRand randMap coeff@(Constant c) = coeff
evalCoefficientRand randMap (Affine c imap) = let evaluated = (sum $ IM.mapWithKey (\i c -> c * (randMap M.! i)) imap) + c
                                              in coeffFromConst evaluated

evalPDERand :: (Num a, Eq a, Random a) => M.Map Int a -> PDE a -> PDE a
evalPDERand randMap (PDECons pdeMap) = pdeFromMap $ M.map (evalCoefficientRand randMap) pdeMap

evalPDESystemRand :: (Num a, Eq a, Random a) => StdGen -> Int -> PDESystem a -> PDESystem a
evalPDESystemRand gen ideps (PDESys pdesys) = PDESys $ fmap (evalPDERand (buildRandomIdepsMap gen ideps)) pdesys


return []
runPDETests = $forAllProperties (quickCheckWithResult stdArgs { maxSuccess = 10000 })
