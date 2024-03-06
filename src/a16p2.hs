-- stack run a16p1 "input/a16p1_sample.txt"

module Main where

import Data.Map (Map, findWithDefault, fromList, map)
import System.Environment (getArgs)
import Prelude hiding (Left, Right)
import Data.Set ( Set, empty, member, insert, size, map )

main :: IO ()
main = do
  getArgs >>= readFile . head >>= print . solve

data Tile = Empty | MirrorUp | MirrorDown | SplitterUD | SplitterLR deriving (Show, Eq, Ord)

data Direction = Up | Down | Left | Right deriving (Show, Eq, Ord)

parseTile :: Char -> Tile
parseTile '.' = Empty
parseTile '/' = MirrorUp
parseTile '\\' = MirrorDown
parseTile '|' = SplitterUD
parseTile '-' = SplitterLR
parseTile _ = error "Invalid tile"

parse :: String -> [[Tile]]
parse = Prelude.map (Prelude.map parseTile) . lines

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

starts :: [[Tile]] -> [((Int, Int), Direction)]
starts ts = t ++ b ++ l ++ r
  where
    l = [((i, 0), Right) | i <- [0 .. length ts - 1]]
    r = [((i, length (head ts) - 1), Left) | i <- [0 .. length ts - 1]]
    t = [((0, j), Down) | j <- [0 .. length (head ts) - 1]]
    b = [((length ts - 1, j), Up) | j <- [0 .. length (head ts) - 1]]

neighbors :: [[Tile]] -> Map ((Int, Int), Direction) [((Int, Int), Direction)]
neighbors ts =
  Data.Map.fromList $
    [ (((i, j), dir), p ((i, j), dir))
      | i <- [0 .. length ts - 1],
        j <- [0 .. length (head ts) - 1],
        dir <- [Up, Down, Left, Right]
    ]
  where
    p :: ((Int, Int), Direction) -> [((Int, Int), Direction)]
    p key = n
      where
        ((i, j), dir) = key
        n' = next dir (ts !! i !! j) (i, j)
        n =
          filter
            ( \((ii, jj), _) ->
                ii >= 0 && jj >= 0 && ii < length ts && jj < length (head ts)
            )
            n'

energized' :: Map ((Int, Int), Direction) [((Int, Int), Direction)] -> ((Int, Int), Direction) -> Set (Int, Int)
energized' ns key = Data.Set.map fst (p [key] Data.Set.empty)
  where
    p :: [((Int, Int), Direction)] -> Set ((Int, Int), Direction) -> Set ((Int, Int), Direction)
    p [] state = state
    p (k:ks) state = if k `Data.Set.member` state then p ks state else p nxt_keys nxt_state
      where
        nxt_keys = ks ++ findWithDefault [] k ns
        nxt_state = Data.Set.insert k state

energized :: [[Tile]] -> Map ((Int, Int), Direction) (Set (Int, Int))
energized ts = Data.Map.fromList $ Prelude.map (\k -> (k, energized' ns k)) $ starts ts
  where
    ns = neighbors ts

solve :: String -> Int
solve = maximum . Data.Map.map size . energized . parse
