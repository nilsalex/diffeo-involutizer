{-# LANGUAGE TemplateHaskell #-}

module MultiIndex where

import Test.QuickCheck
import Data.Maybe
import qualified Data.Map.Strict as M

data MultiIndex = MIx Int Int (M.Map Int Int) deriving (Show, Eq)

instance Ord MultiIndex where
    (MIx len order indexMap) <= (MIx len' order' indexMap')
        | order < order' = True
        | order > order' = False
        | order == order' = indexMap <= indexMap'

empty :: Int -> MultiIndex
empty len 
        | len > 0 = MIx len 0 M.empty
        | otherwise = undefined

single :: Int -> Int -> MultiIndex
single len index
        | len > 0 && index < len = MIx len 1 (M.singleton index 1)
        | otherwise = undefined

addIndex :: Int -> MultiIndex -> MultiIndex
addIndex index m@(MIx len order indexMap)
    | index < len && index >= 0 && len > 0 = MIx len (order + 1) (M.insertWith (+) index 1 indexMap)
    | otherwise = undefined 

fromList :: Int -> [Int] -> MultiIndex
fromList len = foldr (\i mIx -> addIndex i mIx) (empty len)

prettyMIx :: MultiIndex -> String
prettyMIx (MIx len order indexMap) 
        | order == 0 = "u"
        | otherwise = "u_[" ++ init (M.foldMapWithKey (\k a -> concat $ replicate a $ (show (k+1) ++ "|")) indexMap) ++ "]"

instance Arbitrary MultiIndex where
    arbitrary = arbitraryMIx

arbitraryMIx :: Gen MultiIndex
arbitraryMIx = do
                 len <- choose (1, 50)
                 order <- choose (0, 100)
                 is <- infiniteListOf $ choose (0, len - 1)
                 return $ fromList len (take order is)

data MultiIndexAndInt = MultiIndexAndInt MultiIndex Int deriving (Eq, Show, Ord)

instance Arbitrary MultiIndexAndInt where
    arbitrary = do
                 mIx@(MIx len _ _) <- arbitraryMIx
                 i <- choose (0, len - 1)
                 return $ MultiIndexAndInt mIx i

prop_order :: MultiIndex -> Bool
prop_order (MIx _ order indexMap) = order == sum indexMap

prop_indexRange :: MultiIndex -> Bool
prop_indexRange (MIx len order indexMap) 
                            | order == 0 = M.null indexMap
                            | otherwise = let Just (maxIndex, _) = M.lookupMax indexMap
                                              Just (minIndex, _) = M.lookupMin indexMap
                                          in maxIndex < len && minIndex >= 0

prop_addIndex :: MultiIndexAndInt -> Bool
prop_addIndex (MultiIndexAndInt mi@(MIx len order indexMap) i) = (len' == len) &&
                                                                 (order' == order + 1) &&
                                                                 (M.size diffMap == 1) &&
                                                                 (M.elems diffMap == [1]) &&
                                                                 (M.keys diffMap == [i])
                                                                    where (MIx len' order' indexMap') = addIndex i mi
                                                                          diffMap = M.filter (0 <) $ M.unionWith (-) indexMap' indexMap


return []
runMIxTests = $forAllProperties (quickCheckWithResult stdArgs { maxSuccess = 10000 })
