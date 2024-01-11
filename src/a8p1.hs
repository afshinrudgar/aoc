module Main where

import           Data.Char (isSpace)
import           Data.Map  (Map, fromList, (!))

main :: IO ()
main = do
  readFile "input/a8p1.txt" >>= print . solve

data Direction = L | R deriving (Show, Eq)

data WastelandMap = WastelandMap {lefts :: Map String String, rights :: Map String String} deriving (Show, Eq)

parseDirection :: Char -> Direction
parseDirection 'L' = L
parseDirection 'R' = R
parseDirection _   = error "Invalid direction"

parseDirections :: String -> [Direction]
parseDirections = map parseDirection

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

splitByDelimiter :: (Eq a) => a -> [a] -> [[a]]
splitByDelimiter _ [] = []
splitByDelimiter sep s = h : (if null rst then [] else splitByDelimiter sep (tail rst))
  where
    (h, rst) = span (/= sep) s

parseMap :: String -> WastelandMap
parseMap s = WastelandMap (fromList $ zip from toLeft) (fromList $ zip from toRight)
  where
    from = map (trim . head . splitByDelimiter '=') (lines s)
    toLeft = map (trim . dropWhile (== '(') . trim . head . splitByDelimiter ',' . head . tail . splitByDelimiter '=') (lines s)
    toRight = map (trim . takeWhile (/= ')') . trim . head . tail . splitByDelimiter ',' . head . tail . splitByDelimiter '=') (lines s)

parse :: String -> (WastelandMap, [Direction])
parse s = (parseMap $ unlines $ drop 2 $ lines s, parseDirections $ head $ lines s)

countTraverse' :: WastelandMap -> [Direction] -> (String -> Bool) -> String -> Int
countTraverse' _ [] _ _ = error "Impossible"
countTraverse' m (d : ds) f here
  | f here = 0
  | d == L = 1 + countTraverse' m (ds ++ [d]) f (lefts m ! here)
  | otherwise = 1 + countTraverse' m (ds ++ [d]) f (rights m ! here)

countTraverse :: WastelandMap -> [Direction] -> Int
countTraverse m ds = countTraverse' m ds (== "ZZZ") "AAA"

solve :: String -> Int
solve s = countTraverse m directions
  where
    (m, directions) = parse s
