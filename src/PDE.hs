{-# LANGUAGE TemplateHaskell #-}

module PDE where

import MultiIndex
import TriangleMap

import System.Random.TF.Instances
import System.Random.TF.Gen
import Test.QuickCheck
import Control.Monad
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import Data.Foldable

data Coefficient a = Null | Constant a | Affine a (IM.IntMap a) deriving (Show, Eq, Ord)

data PDE a = PDECons Int (M.Map MultiIndex (Coefficient a)) deriving (Show, Eq, Ord)

data PDESystem a = PDESys Int (S.Seq (PDE a)) deriving (Show, Eq, Ord)

instance (Num a, Eq a, Arbitrary a) => Arbitrary (Coefficient a) where
    arbitrary = do 
                 coeffType <- (choose (0, 4) :: Gen Int)
                 randomC <- arbitrary
                 randomM <- arbitrary
                 randomI <- arbitrary
                 let coeff = case coeffType of 0 -> coeffFromConst randomC
                                               1 -> coeffFromMap randomM
                                               2 -> coeffFromConstMap randomC randomM
                                               3 -> coeffFromConstIdep randomC randomI
                                               4 -> Null
                                               _ -> undefined
                 return coeff

coeffConstraint :: (Num a, Eq a) => Coefficient a -> Bool
coeffConstraint Null = True
coeffConstraint (Constant 0) = False
coeffConstraint (Constant _) = True
coeffConstraint (Affine _ imap) = if IM.size imap > 0 then True else False

pdeConstraint :: (Num a, Eq a) => PDE a -> Bool
pdeConstraint (PDECons ideps pdeMap) = (ideps > 0) &&
                                       (all coeffConstraint pdeMap) &&
                                       (all (\mIx -> mIxConstraint mIx && mIxLength mIx == ideps) $ M.keys pdeMap)

coeffFromConst :: (Eq a, Num a) => a -> Coefficient a
coeffFromConst 0 = Null
coeffFromConst c = Constant c

prop_coeffFromConst :: (Eq a, Num a) => a -> Bool
prop_coeffFromConst = coeffConstraint . coeffFromConst
                        
coeffFromMap :: (Num a, Eq a) => IM.IntMap a -> Coefficient a
coeffFromMap imap = if nonZero then Affine 0 nonZeroMap else Null
                    where nonZeroMap = IM.filter ((/=) 0) imap
                          nonZero = not (IM.null nonZeroMap)

prop_coeffFromMap :: (Num a, Eq a) => IM.IntMap a -> Bool
prop_coeffFromMap = coeffConstraint . coeffFromMap

coeffFromConstMap :: (Num a, Eq a) => a -> IM.IntMap a -> Coefficient a
coeffFromConstMap c imap = if nonZero then Affine c nonZeroMap else coeffFromConst c
                           where nonZeroMap = IM.filter ((/=) 0) imap
                                 nonZero = not (IM.null nonZeroMap)

prop_coeffFromConstMap :: (Num a, Eq a) => a -> IM.IntMap a -> Bool
prop_coeffFromConstMap c = coeffConstraint . coeffFromConstMap c

coeffFromConstIdep :: (Eq a, Num a) => a -> Int -> Coefficient a
coeffFromConstIdep 0 _ = Null
coeffFromConstIdep c i = coeffFromMap $ IM.singleton i c

prop_coeffFromConstIdep :: (Eq a, Num a) => a -> Int -> Bool
prop_coeffFromConstIdep c = coeffConstraint . coeffFromConstIdep c

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

prop_addCoefficients :: (Num a, Eq a) => Coefficient a -> Coefficient a -> Bool
prop_addCoefficients c1 c2 = coeffConstraint $ addCoefficients c1 c2

coeffDerivative :: (Num a, Eq a) => Int -> Coefficient a -> Coefficient a
coeffDerivative _ Null = Null
coeffDerivative _ (Constant _) = Null
coeffDerivative derivative (Affine _ imap) = case IM.lookup derivative imap of Nothing  -> Null
                                                                               (Just c) -> coeffFromConst c

prop_coeffDerivative :: (Num a, Eq a) => Int -> Coefficient a -> Bool
prop_coeffDerivative derivative = coeffConstraint . coeffDerivative derivative

prettyCoefficient :: (Show a, Num a, Eq a, Ord a) => Coefficient a -> String
prettyCoefficient Null = "0"
prettyCoefficient (Constant c) = show c
prettyCoefficient (Affine 0 imap) = let parentheses = IM.size imap > 1 in
                                        (if parentheses then "(" else "") ++
                                            (prettyCoefficientLinear imap) ++
                                            (if parentheses then ")" else "")
prettyCoefficient (Affine c imap) = "(" ++ show c ++ (prettyCoefficientLinearWithSign imap) ++ ")"

prettyCoefficientWithSign :: (Show a, Num a, Eq a, Ord a) => Coefficient a -> String
prettyCoefficientWithSign Null = " + 0"
prettyCoefficientWithSign (Constant c)
        | c < 0 = " - " ++ show (abs c)
        | c > 0 = " + " ++ show c
prettyCoefficientWithSign (Affine 0 imap) = if IM.size imap == 1 then prettyCoefficientLinearWithSign imap
                                                                 else " + (" ++ prettyCoefficientLinear imap ++ ")"
prettyCoefficientWithSign (Affine c imap) = " + (" ++ show c ++ (prettyCoefficientLinearWithSign imap) ++ ")"

prettyCoefficientLinearWithSign :: (Show a, Num a, Ord a) => IM.IntMap a -> String
prettyCoefficientLinearWithSign = IM.foldMapWithKey (\i c -> (if signum c < 0 then " - " else " + ") ++ show (abs c) ++ " x_" ++ show (i+1))

prettyCoefficientLinear :: (Show a, Num a, Ord a) => IM.IntMap a -> String
prettyCoefficientLinear imap = let (Just (_, c)) = IM.lookupMin imap
                                   dropFun = if c < 0 then drop 1
                                                        else drop 3
                               in dropFun $
                                  IM.foldMapWithKey (\i c -> (if signum c < 0 then " - " else " + ") ++ show (abs c) ++ " x_" ++ show (i+1)) $
                                  imap

emptyPDE :: Num a => Int -> PDE a
emptyPDE ideps
            | ideps <= 0 = undefined
            | otherwise = PDECons ideps M.empty

prop_emptyPDE :: Positive Int -> Bool
prop_emptyPDE = (pdeConstraint :: PDE Int -> Bool) . emptyPDE . getPositive

pdeFromMap :: (Num a, Eq a) => Int -> M.Map MultiIndex (Coefficient a) -> PDE a
pdeFromMap ideps
            | ideps <= 0 = undefined
            | otherwise  = PDECons ideps . (M.filter (not . isZero))

genPDEMap :: Gen (Int, M.Map MultiIndex (Coefficient Int))
genPDEMap = do
                len <- choose (1, 50)
                pdeMap <- genPDEMapLen len
                return (len, pdeMap)

genPDEMapLen :: Int -> Gen (M.Map MultiIndex (Coefficient Int))
genPDEMapLen len = liftM2 (\a b -> M.fromList $ zip a b) (listOf (genMIx len)) (arbitrary :: Gen [Coefficient Int])
                
prop_pdeFromMap :: Property
prop_pdeFromMap = forAll genPDEMap $ (\(ideps, pdeMap) -> pdeConstraint $ pdeFromMap ideps pdeMap)

fromPDESys :: Num a => PDESystem a -> S.Seq (PDE a)
fromPDESys (PDESys _ sysSeq) = sysSeq

pdeFromConstDep :: (Num a, Eq a) => Int -> a -> PDE a
pdeFromConstDep ideps 0 = emptyPDE ideps
pdeFromConstDep ideps c = pdeFromMap ideps $ M.fromList [(empty ideps, coeffFromConst c)]

prop_pdeFromConstDep :: Property
prop_pdeFromConstDep = forAll
                       ((arbitrary :: Gen (Int, Int)) `suchThat` \(ideps, _) -> ideps > 0) $
                       \(ideps, c) -> pdeConstraint $ pdeFromConstDep ideps c

pdeFromConstIdepDerivative :: (Num a, Eq a) => Int -> a -> Int -> Int -> PDE a
pdeFromConstIdepDerivative ideps 0 _ _ = pdeFromMap ideps M.empty
pdeFromConstIdepDerivative ideps c i d = pdeFromMap ideps $ M.singleton (single ideps d) (coeffFromConstIdep c i)

pdeFromConstDerivative :: (Num a, Eq a) => Int -> a -> Int -> PDE a
pdeFromConstDerivative ideps 0 _ = pdeFromMap ideps M.empty
pdeFromConstDerivative ideps c d = pdeFromMap ideps $ M.singleton (single ideps d) (coeffFromConst c)

prop_pdeFromConstIdepDerivative :: Property
prop_pdeFromConstIdepDerivative = forAll
                                  ((arbitrary :: Gen (Int, Int, Int, Int)) `suchThat` \(ideps, _, i, d) -> ideps > 0 &&
                                                                                                           i >= 0 &&
                                                                                                           d >= 0 &&
                                                                                                           i < ideps &&
                                                                                                           d < ideps) $
                                  \(ideps, c, i, d) -> pdeConstraint $ pdeFromConstIdepDerivative ideps c i d

getPDEMap :: PDE a -> M.Map MultiIndex (Coefficient a)
getPDEMap (PDECons _ pdeMap) = pdeMap

getIdeps :: PDE a -> Int
getIdeps (PDECons ideps _) = ideps

addPDEs :: (Num a, Eq a) => PDE a -> PDE a -> PDE a
addPDEs (PDECons lenA pdeMapA) (PDECons lenB pdeMapB)
            | lenA /= lenB = undefined
            | otherwise = pdeFromMap lenA $ M.unionWith addCoefficients pdeMapA pdeMapB

prop_addPDEs :: Property
prop_addPDEs = forAll (do len <- choose (1, 50)
                          pde1 <- genPDEMapLen len
                          pde2 <- genPDEMapLen len
                          return (pdeFromMap len pde1, pdeFromMap len pde2))
               $ \(pde1, pde2) -> let pdeSum = addPDEs pde1 pde2
                                  in pdeConstraint pdeSum

maybeCoeffToConstant :: (Num a) => Maybe (Coefficient a) -> a
maybeCoeffToConstant Nothing = 0
maybeCoeffToConstant (Just Null) = 0
maybeCoeffToConstant (Just (Constant c)) = c
maybeCoeffToConstant _ = undefined

pdeToConstList :: (Num a) => PDE a -> [a]
pdeToConstList (PDECons ideps pdeMap) = map (\idep -> let coeff = M.lookup (single ideps idep) pdeMap
                                                      in maybeCoeffToConstant coeff) [0..ideps-1]

pdeSystemToConstMatrix :: (Num a) => PDESystem a -> [[a]]
pdeSystemToConstMatrix (PDESys _ pdeSeq) = map pdeToConstList $ toList pdeSeq

pdeOrder :: PDE a -> Int
pdeOrder = M.foldlWithKey (\order mIx _ -> max order (mIxOrder mIx)) 0 . getPDEMap

pdeMinOrder :: PDE a -> Int
pdeMinOrder = M.foldlWithKey (\order mIx _ -> min order (mIxOrder mIx)) 999 . getPDEMap

pdeSystemOrder :: PDESystem a -> Int
pdeSystemOrder = foldl (\order pde -> max order (pdeOrder pde)) 0 . getPDESequence

pdeSystemMinOrder :: PDESystem a -> Int
pdeSystemMinOrder = foldl (\order pde -> min order (pdeMinOrder pde)) 999 . getPDESequence

pdeSymbol :: PDE a -> PDE a
pdeSymbol pde@(PDECons ideps pdeMap) = let order = pdeOrder pde
                                       in PDECons ideps $ M.filterWithKey (\mIx _ -> mIxOrder mIx == order) pdeMap

pdeSystemSymbol :: PDESystem a -> PDESystem a
pdeSystemSymbol pdeSys@(PDESys ideps pdeSeq) = let order = pdeSystemOrder pdeSys
                                                   subSeq = S.filter (\pde -> pdeOrder pde == order) pdeSeq
                                               in PDESys ideps $ fmap pdeSymbol subSeq

prettyPDE :: (Num a, Eq a, Show a, Ord a) => PDE a -> String
prettyPDE pde = let (x:y:z:xs) = M.foldMapWithKey (\mIx coeff -> (prettyCoefficientWithSign coeff) ++ " " ++ (prettyMIx mIx)) (getPDEMap pde) ++ "\n"
                in if y == '+' then xs else y:z:xs

getPDESequence :: PDESystem a -> S.Seq (PDE a)
getPDESequence (PDESys _ pdeSeq) = pdeSeq

getSysIdeps :: (Num a, Eq a) => PDESystem a -> Int
getSysIdeps (PDESys ideps _) = ideps

prettyPDESystem :: (Num a, Eq a, Show a, Ord a) => PDESystem a -> String
prettyPDESystem (PDESys _ pdeseq) = foldMap (\pde -> prettyPDE pde) pdeseq

concatPDESystems :: PDESystem a -> PDESystem a -> PDESystem a
concatPDESystems (PDESys i1 ps1) (PDESys i2 ps2)
                    | i1 /= i2 || i1 == 0 = undefined
                    | otherwise = PDESys i1 $ ps1 S.>< ps2

buildRandomIdepsMap :: (Num a, Eq a, RandomGen r) => r -> Int -> M.Map Int a
buildRandomIdepsMap gen ideps = M.fromList $ zip [0..ideps-1] $ map fromIntegral $ (randomRs (-500, 500) gen :: [Integer])

evalCoefficient :: (Num a, Eq a) => M.Map Int a -> Coefficient a -> Coefficient a
evalCoefficient _ Null = Null
evalCoefficient _ coeff@(Constant _) = coeff
evalCoefficient randMap (Affine c imap) = let evaluated = (sum $ IM.mapWithKey (\i c' -> c' * (randMap M.! i)) imap) + c
                                          in coeffFromConst evaluated

evalPDE :: (Num a, Eq a) => M.Map Int a -> PDE a -> PDE a
evalPDE randMap pde = pdeFromMap (getIdeps pde) $ M.map (evalCoefficient randMap) $ getPDEMap pde

evalPDESystem :: (Num a, Eq a) => M.Map Int a -> PDESystem a -> PDESystem a
evalPDESystem randMap (PDESys ideps pdesys) = PDESys ideps $ fmap (evalPDE randMap) pdesys

evalPDESystemRand :: (Num a, Eq a, RandomGen b) => b -> PDESystem a -> PDESystem a
evalPDESystemRand gen pdesys@(PDESys ideps _) = evalPDESystem (buildRandomIdepsMap gen ideps) pdesys

prettyPDEMatrix :: (Num a, Eq a, Show a, Ord a) => PDESystem a -> String
prettyPDEMatrix (PDESys ideps pdeSequence) = let tMap = buildTriangleMap ideps
                                                 tMap3 = buildTriangleMap3 ideps
                                             in S.foldMapWithIndex (prettyPDEMatrixRow $ toNum tMap tMap3) pdeSequence

prettyPDEMatrixSecond :: (Num a, Eq a, Show a, Ord a) => PDESystem a -> String
prettyPDEMatrixSecond (PDESys ideps pdeSequence) = let tMap = buildTriangleMap ideps
                                                   in S.foldMapWithIndex (prettyPDEMatrixRow $ toSecond tMap) pdeSequence

prettyPDEMatrixRow :: (Num a, Eq a, Show a, Ord a) => (MultiIndex -> Int) -> Int -> PDE a -> String
prettyPDEMatrixRow f eqnum (PDECons _ pdeMap) = M.foldMapWithKey
                                                        (\mIx coeff -> "(" ++
                                                                       show (eqnum+1) ++
                                                                       ", " ++
                                                                       show (f mIx + 1) ++
                                                                       ") = " ++
                                                                       prettyCoefficient coeff ++
                                                                       "\n")
                                                          pdeMap

return []
runPDETests :: IO Bool
runPDETests = $forAllProperties (quickCheckWithResult stdArgs { maxSuccess = 10000 })
