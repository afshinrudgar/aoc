module Main where

import Data.Char (isSpace)
import Data.List (sortBy)

main :: IO ()
main = do
  readFile "input/a5p1.txt" >>= print . solve

data Name
  = Seed
  | Soil
  | Fertilizer
  | Water
  | Light
  | Temperature
  | Humidity
  | Location
  deriving (Show, Eq, Ord, Enum)

type Range = (Int, Int)

type Map' = (Name, Name, [(Range, Range)])

type Mappings = [Map']

buildMappings :: [Map'] -> Mappings
buildMappings [] = []
buildMappings xs = sortBy (\(a, _, _) (b, _, _) -> compare a b) xs

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

splitByDelimiter :: (Eq a) => a -> [a] -> [[a]]
splitByDelimiter _ [] = []
splitByDelimiter sep s = h : (if null rst then [] else splitByDelimiter sep (tail rst))
  where
    (h, rst) = span (/= sep) s

parseSeeds :: String -> [Int]
parseSeeds s = map (read . trim) (splitByDelimiter ' ' r)
  where
    (_, r') = span (/= ':') s
    r = drop 2 r'

parseRange :: String -> (Range, Range)
parseRange s = ((src, offset), (dest, offset))
  where
    parts = splitByDelimiter ' ' s
    dest = read (head parts)
    src = read (parts !! 1)
    offset = read (parts !! 2)

parseName :: String -> Name
parseName "seed" = Seed
parseName "soil" = Soil
parseName "fertilizer" = Fertilizer
parseName "water" = Water
parseName "light" = Light
parseName "temperature" = Temperature
parseName "humidity" = Humidity
parseName "location" = Location
parseName _ = error "Unknown name"

parseMap :: [String] -> Map'
parseMap ss = (mapFrom, mapTo, ranges)
  where
    (title, _) = span (/= ' ') (head ss)
    mapInfo = splitByDelimiter '-' title
    mapFrom = parseName $ head mapInfo
    mapTo = parseName $ mapInfo !! 2
    ranges = map parseRange (tail ss)

parseMappings :: [String] -> [Map']
parseMappings ss = map parseMap (splitByDelimiter "" ss)

parse :: String -> (Mappings, [Int])
parse s = (buildMappings mappings, seeds)
  where
    ls = lines s
    seeds = parseSeeds (head ls)
    mappings = parseMappings (map trim $ drop 2 ls)

inRange :: Range -> Int -> Bool
inRange (start, offset) x = x >= start && x < start + offset

toNextValue :: (Range, Range) -> Int -> Int
toNextValue (fromRange, toRange) x =
  if inRange fromRange x
    then x - fst fromRange + fst toRange
    else x

toNextValueM :: [(Range, Range)] -> Int -> Int
toNextValueM [] v = v
toNextValueM ((fromRange, toRange) : xs) v =
  if inRange fromRange v
    then v - fst fromRange + fst toRange
    else toNextValueM xs v

toNextValueM' :: [(Range, Range)] -> [Int] -> [Int]
toNextValueM' ranges = map (toNextValueM ranges)

next :: Mappings -> Name -> [Int] -> [Int]
next [] _ _ = []
next _ _ [] = []
next ((mapFrom, _, ranges) : mappingRest) name vals =
  if name /= mapFrom
    then next mappingRest name vals
    else toNextValueM' ranges vals

allPathsToLocation :: Mappings -> [Int] -> [Int]
allPathsToLocation [] _ = []
allPathsToLocation _ [] = []
allPathsToLocation m seeds =
  location
  where
    soil = next m Seed seeds
    fertilizer = next m Soil soil
    water = next m Fertilizer fertilizer
    light = next m Water water
    temperature = next m Light light
    humidity = next m Temperature temperature
    location = next m Humidity humidity

solve :: String -> Int
solve s = minimum (allPathsToLocation mappings seeds)
  where
    (mappings, seeds) = parse s
