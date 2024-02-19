module Main where

import Data.List (transpose)

main :: IO ()
main = do
  readFile "input/a14p1.txt" >>= print . solve


tilt' ::  String -> String
tilt' [] = []
tilt' ('#':xs) = '#':tilt' xs
tilt' xs = replicate cnt 'O' ++ replicate (length h - cnt) '.' ++ tilt' rst
    where
        (h, rst) = span (/= '#') xs
        cnt = (length . filter (== 'O')) h

tilt :: [String] -> [String]
tilt = transpose . map tilt' . transpose

load :: [String] -> Int
load xs = sum $ zipWith (\s c ->  length (filter (=='O') s) * c) (reverse xs) [1..]

solve :: String -> Int
solve = load . tilt . lines