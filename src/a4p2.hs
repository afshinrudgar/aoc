{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Data.Char (isSpace)

main :: IO ()
main = do
  readFile "input/a4p2.txt" >>= print . solve

type Card = (Int, [Int], [Int])

cardId :: Card -> Int
cardId (id_, _, _) = id_

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
readCards s = (id_, ws, hs)
  where
    (id', r') = span (/= ':') s
    (ws', hs') = span (/= '|') (trim $ tail r')
    id_ = read (trim $ drop 5 id') :: Int
    hs = map read $ filter (/= "") $ splitByDelimiter ' ' (trim $ tail hs')
    ws = map read $ filter (/= "") $ splitByDelimiter ' ' (trim ws')

countMatch :: Card -> Int
countMatch (_, ws, hs) = length (filter (`elem` ws) hs)

produceCardCounts :: [(Card, Integer)] -> [(Card, Integer)]
produceCardCounts [] = []
produceCardCounts ((card, count) : restOfCards) = (card, count) : produceCardCounts rest
  where
    points = countMatch card
    rest = map (\(crd, cnt) -> (crd, cnt + count)) (take points restOfCards) ++ drop points restOfCards

solve :: String -> Integer
solve = sum . map snd . produceCardCounts . map ((,1) . readCards) . lines
