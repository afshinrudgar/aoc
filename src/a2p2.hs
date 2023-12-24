-- Determine which games would have been possible if the bag had been loaded
-- with only 12 red cubes, 13 green cubes, and 14 blue cubes. What is the sum of the IDs of those games?

module Main (main) where

import Data.Char (isSpace)
import Data.List (isSuffixOf)

-- Read a file line by line
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

main :: IO ()
main = do
  readLines "input/a2p2.txt" >>= print . solve

-- A game set: (red, green, blue)
type Set = (Int, Int, Int)

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

splitByDelimiter :: Char -> String -> [String]
splitByDelimiter _ "" = []
splitByDelimiter sep s = h : (if rst == "" then [] else splitByDelimiter sep (tail rst))
  where
    (h, rst) = span (/= sep) s

parseCube :: String -> String -> Int
parseCube color s = if color `isSuffixOf` s then read (takeWhile (/= ' ') s) :: Int else 0

readSet :: String -> Set
readSet s = (redCubes, greenCubes, blueCubes)
  where
    ps = map trim $ splitByDelimiter ',' s
    redCubes = maximum $ map (parseCube "red") ps
    greenCubes = maximum $ map (parseCube "green") ps
    blueCubes = maximum $ map (parseCube "blue") ps

-- Game type: (id, sets)
type Game = (Int, [Set])

readGame :: String -> Game
readGame s = (gameId, sets)
  where
    gameId = read (dropWhile (/= ' ') p1) :: Int
    sets = map (readSet . trim) (splitByDelimiter ';' (tail p2))
    (p1, p2) = span (/= ':') s

red :: Set -> Int
red (redCubes, _, _) = redCubes

reds :: Game -> [Int]
reds (_, sets) = map red sets

green :: Set -> Int
green (_, greenCubes, _) = greenCubes

greens :: Game -> [Int]
greens (_, sets) = map green sets

blue :: Set -> Int
blue (_, _, blueCubes) = blueCubes

blues :: Game -> [Int]
blues (_, sets) = map blue sets

minSet :: Game -> Set
minSet g = (maximum $ reds g, maximum $ greens g, maximum $ blues g)

calcPower :: Set -> Int
calcPower (r, g, b) = r * g * b

solve :: [String] -> Int
solve = sum . map (calcPower . minSet . readGame)
