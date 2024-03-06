-- stack run a16p1 "input/a16p1_sample.txt"

module Main where

import Data.List (group, sort)
import System.Environment (getArgs)
import Prelude hiding (Left, Right)

main :: IO ()
main = do
  getArgs >>= readFile . head >>= print . solve

data Tile = Empty | MirrorUp | MirrorDown | SplitterUD | SplitterLR deriving (Show, Eq)

data Direction = Up | Down | Left | Right deriving (Show, Eq)

parseTile :: Char -> Tile
parseTile '.' = Empty
parseTile '/' = MirrorUp
parseTile '\\' = MirrorDown
parseTile '|' = SplitterUD
parseTile '-' = SplitterLR
parseTile _ = error "Invalid tile"

parse :: String -> [[Tile]]
parse = map (map parseTile) . lines

next :: Direction -> Tile -> (Int, Int) -> [((Int, Int), Direction)]
-- mirror up
next Right MirrorUp (i, j) = [((i - 1, j), Up)]
next Left MirrorUp (i, j) = [((i + 1, j), Down)]
next Up MirrorUp (i, j) = [((i, j + 1), Right)]
next Down MirrorUp (i, j) = [((i, j - 1), Left)]
-- mirror down
next Right MirrorDown (i, j) = [((i + 1, j), Down)]
next Left MirrorDown (i, j) = [((i - 1, j), Up)]
next Up MirrorDown (i, j) = [((i, j - 1), Left)]
next Down MirrorDown (i, j) = [((i, j + 1), Right)]
-- splitter up down
next Right SplitterUD (i, j) = [((i - 1, j), Up), ((i + 1, j), Down)]
next Left SplitterUD (i, j) = [((i - 1, j), Up), ((i + 1, j), Down)]
next Up SplitterUD (i, j) = [((i - 1, j), Up)]
next Down SplitterUD (i, j) = [((i + 1, j), Down)]
-- splitter left right
next Right SplitterLR (i, j) = [((i, j + 1), Right)]
next Left SplitterLR (i, j) = [((i, j - 1), Left)]
next Up SplitterLR (i, j) = [((i, j - 1), Left), ((i, j + 1), Right)]
next Down SplitterLR (i, j) = [((i, j - 1), Left), ((i, j + 1), Right)]
-- Empty
next Right Empty (i, j) = [((i, j + 1), Right)]
next Left Empty (i, j) = [((i, j - 1), Left)]
next Up Empty (i, j) = [((i - 1, j), Up)]
next Down Empty (i, j) = [((i + 1, j), Down)]

paths :: [[Tile]] -> [(Int, Int)]
paths tiles = map fst (p tiles ((0, 0), Right) [])
  where
    p :: [[Tile]] -> ((Int, Int), Direction) -> [((Int, Int), Direction)] -> [((Int, Int), Direction)]
    p ts ((i, j), dir) state =
      if i < 0 || j < 0 || i >= length ts || j >= length (head ts)
        then state
        else foldl (flip (p ts)) (((i, j), dir) : state) (filter (not . (`elem` state)) (next dir (ts !! i !! j) (i, j)))

uniqueCount :: (Ord a) => [a] -> Int
uniqueCount = length . group . sort

solve :: String -> Int
solve = uniqueCount . paths . parse
