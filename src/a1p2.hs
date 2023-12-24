module Main (main) where

import Data.List (tails)
import Data.Maybe (isJust, mapMaybe)

main :: IO ()
main = do
  readLines "input/a1p2.txt" >>= print . solve

solve :: [String] -> Int
solve = sum . map toNumber

startsWithNumStr :: [Char] -> Maybe Int
startsWithNumStr ('o' : 'n' : 'e' : _) = Just 1
startsWithNumStr ('t' : 'w' : 'o' : _) = Just 2
startsWithNumStr ('t' : 'h' : 'r' : 'e' : 'e' : _) = Just 3
startsWithNumStr ('f' : 'o' : 'u' : 'r' : _) = Just 4
startsWithNumStr ('f' : 'i' : 'v' : 'e' : _) = Just 5
startsWithNumStr ('s' : 'i' : 'x' : _) = Just 6
startsWithNumStr ('s' : 'e' : 'v' : 'e' : 'n' : _) = Just 7
startsWithNumStr ('e' : 'i' : 'g' : 'h' : 't' : _) = Just 8
startsWithNumStr ('n' : 'i' : 'n' : 'e' : _) = Just 9
startsWithNumStr _ = Nothing

startsWithNum :: [Char] -> Maybe Int
startsWithNum ('1' : _) = Just 1
startsWithNum ('2' : _) = Just 2
startsWithNum ('3' : _) = Just 3
startsWithNum ('4' : _) = Just 4
startsWithNum ('5' : _) = Just 5
startsWithNum ('6' : _) = Just 6
startsWithNum ('7' : _) = Just 7
startsWithNum ('8' : _) = Just 8
startsWithNum ('9' : _) = Just 9
startsWithNum _ = Nothing

firstNum :: [Char] -> Int
firstNum = head . mapMaybe (\s -> if isJust (startsWithNumStr s) then startsWithNumStr s else startsWithNum s) . tails

lastNum :: [Char] -> Int
lastNum = head . mapMaybe (\s -> if isJust (startsWithNumStr s) then startsWithNumStr s else startsWithNum s) . reverse . tails

toNumber :: [Char] -> Int
toNumber s = 10 * firstNum s + lastNum s

-- Read a file line by line
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile
