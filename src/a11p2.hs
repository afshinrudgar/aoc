module Main where

import           Data.List (transpose)


main :: IO ()
main = do
    readFile "input/a11p2.txt" >>= print . solve

solve :: String -> Int
solve = sum . distances . lines

rate :: Int
rate = 1000000

empties :: [String] -> [Int]
empties = map (\x -> if all (== '.') x then 1 else 0)

expansions :: [String] -> [Int]
expansions = scanl1 (+) . empties

galaxies :: [String] -> [(Int, Int)]
galaxies m = [(x, y) | (x, row) <- zip [0..] m, (y, c) <- zip [0..] row, c == '#']

tuples :: [a] -> [(a, a)]
tuples []       = []
tuples (x : xs) = map (\y -> (x,y)) xs ++ tuples xs

distance :: [Int] -> [Int] -> ((Int, Int), (Int, Int)) -> Int
distance r c ((x1, y1), (x2, y2)) = abs (x1 - x2) + re + abs (y1 - y2) + ce
    where
        re = abs (r !! x1 - r !! x2) * (rate - 1) -- not to count it twice
        ce = abs (c !! y1 - c !! y2) * (rate - 1)

distances :: [String] -> [Int]
distances m = map (distance r c) (tuples (galaxies m))
    where
        r = expansions m
        c = expansions (transpose m)
