module Main where

import           Data.List (transpose)

main :: IO ()
main = do
    readFile "input/a11p1.txt" >>= print . solve

solve :: String -> Int
solve = sum . distances . expand . lines

expand' :: [String] -> [String]
expand' []     = []
expand' (x:xs) = if all (=='.') x then x:x:expand' xs else x:expand' xs

expand :: [String] -> [String]
expand = transpose . expand' . transpose . expand'

galaxies :: [String] -> [(Int, Int)]
galaxies m = [(x, y) | (y, row) <- zip [0..] m, (x, c) <- zip [0..] row, c == '#']

tuples :: [a] -> [(a, a)]
tuples []       = []
tuples (x : xs) = map (\y -> (x,y)) xs ++ tuples xs

distance :: ((Int, Int), (Int, Int)) -> Int
distance ((x1, y1), (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)

distances :: [String] -> [Int]
distances = map distance . tuples . galaxies
