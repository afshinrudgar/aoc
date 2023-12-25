{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Char (isDigit)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
  readFile "input/a3p2.txt" >>= print . solve

-- type Part position (partId, line, start, end) or PartSymbol (start, end)
type PartId = Int

type Position = (Int, Int, Int)

data Slot = PartSlot PartId Position | SymbolSlot Char Position deriving (Show)

readSlots :: Int -> Int -> String -> [Slot]
readSlots _ _ "" = []
readSlots l c ('.' : xs) = readSlots l (c + 1) xs
readSlots l c xs =
  if not (null partId)
    then PartSlot (read partId :: Int) (l, c, c + length partId) : readSlots l (c + length partId) rest
    else SymbolSlot (head xs) (l, c, c + 1) : readSlots l (c + 1) (tail xs)
  where
    (partId, rest) = span isDigit xs

readAllSlots :: [String] -> [Slot]
readAllSlots xs = concatMap (\(lnum, line) -> readSlots lnum 1 line) $ zip [1 ..] xs

partPositions :: [Slot] -> [Position]
partPositions [] = []
partPositions (PartSlot _ pos : xs) = pos : partPositions xs
partPositions (_ : xs) = partPositions xs

isAdjacent :: Position -> Position -> Bool
isAdjacent (l1, s1, e1) (l2, s2, e2)
  | l1 == l2 = s1 == e2 || e1 == s2
  | abs (l1 - l2) == 1 = max s1 s2 <= min e1 e2
  | otherwise = False

extractGearRatio :: [(PartId, Position)] -> Position -> Maybe Int
extractGearRatio [] _ = Nothing
extractGearRatio ps pos =
  if length ff == 2
    then Just (fst (head ff) * fst (head (tail ff)))
    else Nothing
  where
    ff = filter (isAdjacent pos . snd) ps

extractGearRatios :: [Slot] -> [Int]
extractGearRatios slots =
  mapMaybe (extractGearRatio parts) symbols
  where
    parts = mapMaybe (\case (PartSlot partId pos) -> Just (partId, pos); _ -> Nothing) slots
    symbols = mapMaybe (\case (SymbolSlot '*' pos) -> Just pos; _ -> Nothing) slots

solve :: String -> Int
solve = sum . extractGearRatios . readAllSlots . lines
