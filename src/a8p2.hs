module Main where

import Data.Char (isSpace)
import Data.Map (Map, fromList, keys, (!))

main :: IO ()
main = do
  readFile "input/a8p2.txt" >>= print . solve

data Direction = L | R deriving (Show, Eq)

data WastelandMap = WastelandMap {lefts :: Map String String, rights :: Map String String} deriving (Show, Eq)

parseDirection :: Char -> Direction
parseDirection 'L' = L
parseDirection 'R' = R
parseDirection _ = error "Invalid direction"

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

path :: String -> WastelandMap -> [Direction] -> String -> [String]
path _ _ [] _ = error "Impossible"
path name m (d : ds) here =
  if d == L
    then here : path name m (ds ++ [d]) (lefts m ! here)
    else here : path name m (ds ++ [d]) (rights m ! here)

endsWith :: Char -> String -> Bool
endsWith ch = (== ch) . last

countTo :: WastelandMap -> [Direction] -> String -> (String -> Bool) -> Int
countTo m ds start f = length $ takeWhile (not . f) (path start m ds start)

countTraverse :: WastelandMap -> [Direction] -> Int
countTraverse m ds = foldl lcm 1 cnts
  where
    aList = filter (endsWith 'A') (keys (lefts m))
    cnts = map (\start -> countTo m ds start (endsWith 'Z')) aList

solve :: String -> Int
solve s = countTraverse m directions
  where
    (m, directions) = parse s
