module Main where

import Data.Foldable (find)
import Data.List (transpose)
import Data.Maybe (fromMaybe, isJust)
import Prelude hiding (cycle)

rounds :: Int
rounds = 1000000000

main :: IO ()
main = do
  readFile "input/a14p2.txt" >>= print . solve

tilt' :: String -> String
tilt' [] = []
tilt' ('#' : xs) = '#' : tilt' xs
tilt' xs = replicate cnt 'O' ++ replicate (length h - cnt) '.' ++ tilt' rst
  where
    (h, rst) = span (/= '#') xs
    cnt = (length . filter (== 'O')) h

tiltNorth :: [String] -> [String]
tiltNorth = transpose . map tilt' . transpose

tiltWest :: [String] -> [String]
tiltWest = map tilt'

tiltSouth :: [String] -> [String]
tiltSouth = reverse . tiltNorth . reverse

tiltEast :: [String] -> [String]
tiltEast = map reverse . tiltWest . map reverse

cycle :: [String] -> [String]
cycle = tiltEast . tiltSouth . tiltWest . tiltNorth

-- spin :: [String] -> [[String]]
-- spin xs = c:spin c
--   where
--     c = cycle xs

format :: [String] -> String
format = unlines

finalize :: [String] -> [String]
finalize s = q [] (1, cycle s)
  where
    q :: [(Int, [String])] -> (Int, [String]) -> [String]
    q xs (n, x) =
      if isJust r
        then snd (xs !! pos)
        else q (xs ++ [(n, x)]) (n + 1, cycle x)
      where
        r = find ((== x) . snd) xs
        m = fst $ fromMaybe (0, []) r
        pos = m + ((rounds - n) `rem` (n - m)) - 1

load :: [String] -> Int
load xs = sum $ zipWith (\s c -> length (filter (== 'O') s) * c) (reverse xs) [1 ..]

solve :: String -> Int
solve = load . finalize . lines