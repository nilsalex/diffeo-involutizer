module TriangleMap where

import qualified Data.Map as Map

buildTriangleMap :: Int -> Map.Map (Int, Int) Int
buildTriangleMap dim = Map.fromList $ triangleList dim

triangleDim :: Int -> Int
triangleDim dim = (dim * (dim + 1)) `div` 2

triangleList :: Int -> [((Int, Int), Int)]
triangleList dim = foldl (triangleListInner dim) [] [0..triangleDim dim - 1]

triangleListInner :: Int -> [((Int, Int), Int)] -> Int -> [((Int, Int), Int)]
triangleListInner _ [] _ = [((0, 0), 0)]
triangleListInner dim ((xp, xi):xs) i = (nextPair dim xp, i) : (xp, xi) : xs

nextPair :: Int -> (Int, Int) -> (Int, Int)
nextPair dim (a, b) = if b < dim - 1 then (a, b + 1) else (a+1, a+1)

buildTriangleMap3 :: Int -> Map.Map (Int, Int, Int) Int
buildTriangleMap3 dim = Map.fromList $ zip [(a, b, c) | a <- [0..dim-1], b <- [a..dim-1], c <- [b..dim-1]]
                                           [0..]
