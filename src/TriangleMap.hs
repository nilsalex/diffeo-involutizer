module TriangleMap where

import qualified Data.Map as Map

buildTriangleMap :: Integer -> Map.Map (Integer, Integer) Integer
buildTriangleMap dim = Map.fromList $ triangleList dim

triangleDim :: Integer -> Integer
triangleDim dim = (dim * (dim + 1)) `div` 2

triangleList :: Integer -> [((Integer, Integer), Integer)]
triangleList dim = foldl (triangleListInner dim) [] [0..triangleDim dim - 1]

triangleListInner :: Integer -> [((Integer, Integer), Integer)] -> Integer -> [((Integer, Integer), Integer)]
triangleListInner dim [] i = [((0, 0), 0)]
triangleListInner dim ((xp, xi):xs) i = (nextPair dim xp, i) : (xp, xi) : xs

nextPair :: Integer -> (Integer, Integer) -> (Integer, Integer)
nextPair dim (a, b) = if b < dim - 1 then (a, b + 1) else (a+1, a+1)
