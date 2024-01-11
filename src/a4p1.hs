module Main where

import           Data.Char (isSpace)

main :: IO ()
main = do
  readFile "input/a4p1.txt" >>= print . solve

type Card = ([Int], [Int])

winners :: Card -> [Int]
winners = fst

hand :: Card -> [Int]
hand = snd

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

splitByDelimiter :: Char -> String -> [String]
splitByDelimiter _ "" = []
splitByDelimiter sep s = h : (if rst == "" then [] else splitByDelimiter sep (tail rst))
  where
    (h, rst) = span (/= sep) s

readCards :: String -> Card
readCards s = (ws, hs)
  where
    (_, r') = span (/= ':') s
    (ws', hs') = span (/= '|') (trim $ tail r')
    hs = map read $ filter (/= "") $ splitByDelimiter ' ' (trim $ tail hs')
    ws = map read $ filter (/= "") $ splitByDelimiter ' ' (trim ws')

calcPoint :: Card -> Int
calcPoint (ws, hs) = if len > 0 then 2 ^ (len - 1) else 0
  where
    len = length (filter (`elem` ws) hs)

solve :: String -> Int
solve = sum . map (calcPoint . readCards) . lines
