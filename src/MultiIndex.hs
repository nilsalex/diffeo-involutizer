{-# LANGUAGE TemplateHaskell #-}

module MultiIndex where

import Test.QuickCheck
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

data MultiIndex = MIx Int Int (IM.IntMap Int) deriving (Show, Eq)

instance Ord MultiIndex where
    (MIx len order indexMap) <= (MIx len' order' indexMap')
        | len /= len' = undefined
        | order < order' = True
        | order > order' = False
        | order == order' = indexMap <= indexMap'
        | otherwise = undefined

instance Arbitrary MultiIndex where
    arbitrary = do
                 len <- choose (1, 50)
                 mIx <- genMIx len
                 return mIx

mIxLength :: MultiIndex -> Int
mIxLength (MIx len _ _) = len

genMIx :: Int -> Gen MultiIndex
genMIx len = do
              order <- choose (0, 100)
              is <- infiniteListOf $ choose (0, len - 1)
              return $ fromList len (take order is)

mIxConstraint :: MultiIndex -> Bool
mIxConstraint (MIx len order indexMap) = len > 0 &&
                                         order >= 0 &&
                                         all (>0) indexMap &&
                                         sum indexMap == order &&
                                         if order == 0 then IM.null indexMap
                                         else let Just (maxIndex, _) = IM.lookupMax indexMap
                                                  Just (minIndex, _) = IM.lookupMin indexMap
                                              in maxIndex < len && minIndex >= 0

empty :: Int -> MultiIndex
empty len 
        | len > 0 = MIx len 0 IM.empty
        | otherwise = undefined

prop_empty :: Positive Int -> Bool
prop_empty = mIxConstraint . empty . getPositive

single :: Int -> Int -> MultiIndex
single len index
        | len > 0 && index < len = MIx len 1 (IM.singleton index 1)
        | otherwise = undefined

genLenOrderPair :: Gen (Int, Int)
genLenOrderPair = (arbitrary :: Gen (Int, Int)) `suchThat` (\(a, b) -> a > b && b >= 0)

prop_single :: Property
prop_single = forAll genLenOrderPair $
              \(len, index) -> let mIx@(MIx len' order' indexMap') = single len index
                               in mIxConstraint mIx && 
                                  len' == len &&
                                  order' == 1 &&
                                  IM.size indexMap' == 1 &&
                                  IM.lookup index indexMap' == Just 1

addIndex :: Int -> MultiIndex -> MultiIndex
addIndex index (MIx len order indexMap)
    | index < len && index >= 0 && len > 0 = MIx len (order + 1) (IM.insertWith (+) index 1 indexMap)
    | otherwise = undefined 

genMIxIndexPair :: Gen (MultiIndex, Int)
genMIxIndexPair = do
                   len <- choose (1, 50)
                   mIx <- genMIx len
                   index <- choose (0, len-1)
                   return (mIx, index)

prop_addIndex :: Property
prop_addIndex = forAll genMIxIndexPair $
                \(mi@(MIx len order indexMap), i) -> let mi'@(MIx len' order' indexMap') = addIndex i mi
                                                         diffMap = IM.filter (0 <) $ IM.unionWith (-) indexMap' indexMap
                                                     in (mIxConstraint mi') &&
                                                        (len' == len) &&
                                                        (order' == order + 1) &&
                                                        (IM.size diffMap == 1) &&
                                                        (IM.elems diffMap == [1]) &&
                                                        (IM.keys diffMap == [i])

fromList :: Int -> [Int] -> MultiIndex
fromList len = foldr (\i mIx -> addIndex i mIx) (empty len)

prop_constraint :: MultiIndex -> Bool
prop_constraint = mIxConstraint

toZerothFirstSecond :: M.Map (Int, Int) Int -> MultiIndex -> Int
toZerothFirstSecond tMap (MIx len order indexMap)
                        | order == 0 = 0
                        | order == 1 = 1 + (head $ IM.keys indexMap)
                        | order == 2 = 1 + len + (tMap M.! case (IM.toList indexMap) of
                                                            (i, 2):[] -> (i, i)
                                                            (i, 1):(j, 1):[] -> (i, j)
                                                            _ -> undefined)
                        | otherwise = undefined

toFirstSecond :: M.Map (Int, Int) Int -> MultiIndex -> Int
toFirstSecond tMap (MIx len order indexMap)
                        | order == 0 = undefined
                        | order == 1 = (head $ IM.keys indexMap)
                        | order == 2 = len + (tMap M.! case (IM.toList indexMap) of
                                                            (i, 2):[] -> (i, i)
                                                            (i, 1):(j, 1):[] -> (i, j)
                                                            _ -> undefined)
                        | otherwise = undefined

prettyMIx :: MultiIndex -> String
prettyMIx (MIx _ order indexMap) 
        | order == 0 = "u"
        | otherwise = "u_[" ++ init (IM.foldMapWithKey (\k a -> concat $ replicate a $ (show (k+1) ++ "|")) indexMap) ++ "]"

return []
runMIxTests :: IO Bool
runMIxTests = $forAllProperties (quickCheckWithResult stdArgs { maxSuccess = 10000 })
