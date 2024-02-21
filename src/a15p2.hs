module Main where

import Data.Char (ord)
import GHC.Arr ((//), Array, array, (!), assocs)


main :: IO ()
main = do
  readFile "input/a15p2.txt" >>= print . solve

hash :: String -> Int
hash = foldl (\curr c -> (curr + ord c) * 17 `rem` 256) 0

type Label = String
data Op = Add Int | Remove deriving (Show, Eq)
data Lens = Lens {label :: Label, focalLengh :: Int} deriving (Show, Eq)

splitByOp :: String -> (Label, Op)
splitByOp xs = (l, op) 
  where
    (l, rst) = span (\x -> x /= '=' && x /= '-') xs
    op = if head rst == '=' then Add (read $ drop 1 rst) else Remove


splitByComma :: String -> [String]
splitByComma [] = []
splitByComma (',':xs) = splitByComma xs
splitByComma xs = h:splitByComma t
    where
        (h, t) = span (/= ',') xs

hashmap :: [(Label, Op)] -> Array Int [Lens]
hashmap = p (array (0, 256) [(i, []) | i <- [0..256]])
  where
    p :: Array Int [Lens] -> [(Label, Op)] -> Array Int [Lens]
    p acc [] = acc
    p acc (x:xs) = p (acc // [(h, nxt (acc ! h) x)]) xs
      where
        h = hash (fst x)

    nxt :: [Lens] -> (Label, Op) -> [Lens]
    nxt lenses (l, Add n) = if l `elem` map label lenses then map (\x -> if label x == l then Lens l n else x) lenses else lenses ++ [Lens l n]
    nxt lenses (l, Remove) = filter (\x -> label x /= l) lenses

focusPower' :: Int -> [Lens] -> Int
focusPower' n lenses = sum (zipWith (\lensNum lens -> lensNum * focalLengh lens * (n+1)) [1..] lenses)

focusPower :: Array Int [Lens] -> Int
focusPower = sum . map (uncurry focusPower') . assocs

solve :: String -> Int
solve = focusPower . hashmap . map splitByOp . splitByComma . filter (/= '\n')
