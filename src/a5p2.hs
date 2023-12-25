module Main where

import Data.Char (isSpace)
import Data.List (sortBy, sortOn)
import Debug.Trace (trace)

main :: IO ()
main = do
  readFile "input/a5p2.txt" >>= print . solve

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

type Range = (Integer, Integer)

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

splitList :: Integer -> [a] -> [[a]]
splitList n l =
  map reverse . reverse . snd $
    foldr
      ( \el cont (countAcc, listAcc) ->
          case listAcc of
            [] -> cont (countAcc, [[el]])
            (h : t) | countAcc < n -> cont (countAcc + 1, (el : h) : t)
            (h : t) -> cont (1, [el] : (h : t))
      )
      id
      l
      (1, [])

parseSeeds :: String -> [Range]
parseSeeds s =
  map (\x -> (head x, head (tail x))) parts
  where
    (_, r') = span (/= ':') s
    r = drop 2 r'
    nums = map (read . trim) (splitByDelimiter ' ' r)
    parts = splitList 2 nums

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

parse :: String -> (Mappings, [Range])
parse s = (buildMappings mappings, seeds)
  where
    ls = lines s
    seeds = parseSeeds (head ls)
    mappings = parseMappings (map trim $ drop 2 ls)

hasOverlap :: Range -> Range -> Bool
hasOverlap (start1, offset1) (start2, offset2) =
  max start1 start2 < min end1 end2
  where
    end1 = start1 + offset1
    end2 = start2 + offset2

-- returns (matched, unmatched)
toNextRange' :: (Range, Range) -> Range -> (Range, [Range])
toNextRange' ((fromStart, fromOffset), (toStart, _)) (inStart, inOffset)
  | fromStart <= inStart && inEnd <= fromEnd =
      ((inStart + offset, inEnd - inStart), [])
  | fromStart <= inStart && fromEnd < inEnd =
      ((inStart + offset, fromEnd - inStart), [(fromEnd, inEnd - fromEnd)])
  | inStart < fromStart && inEnd <= fromEnd =
      ((fromStart + offset, inEnd - fromStart), [(inStart, fromStart - inStart)])
  | otherwise =
      ( (max fromStart inStart + offset, min fromEnd inEnd - max fromStart inStart),
        [(inStart, fromStart - inStart), (fromEnd, inEnd - fromEnd)]
      )
  where
    fromEnd = fromStart + fromOffset
    inEnd = inStart + inOffset
    offset = toStart - fromStart

toNextRange :: [(Range, Range)] -> Range -> [Range]
toNextRange [] r = [r]
toNextRange (x : xs) r =
  trace ("toNextRange x:" ++ show x ++ "\tr: " ++ show r ++ "\txs: " ++ show xs) $
    if hasOverlap (fst x) r
      then fst (toNextRange' x r) : concatMap (toNextRange xs) (snd $ toNextRange' x r)
      else toNextRange xs r

toNextValueM' :: [(Range, Range)] -> [Range] -> [Range]
toNextValueM' ranges = concatMap (toNextRange ranges)

next :: Mappings -> Name -> [Range] -> [Range]
next [] _ _ = []
next _ _ [] = []
next ((mapFrom, _, ranges) : mappingRest) name vals =
  if name /= mapFrom
    then next mappingRest name vals
    else toNextValueM' ranges vals

allPathsToLocation :: Mappings -> [Range] -> [Range]
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

solve :: String -> Integer
solve s = fst $ head $ sortOn fst (allPathsToLocation mappings seeds)
  where
    (mappings, seeds) = parse s
