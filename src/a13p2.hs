module Main where

import Data.List (transpose)

main :: IO ()
main = do
  readFile "input/a13p2.txt" >>= print . solve

diff :: String -> String -> Int
diff [] [] = 0
diff _ [] = error "Invalid input"
diff [] _ = error "Invalid input"
diff (a:as) (b:bs) = (if a == b then 0 else 1) + diff as bs

smudge :: Int -> [(String, String)] -> Bool
smudge threshold = (==threshold) . sum . map (uncurry diff)

isReflection :: [String] -> Int -> Bool
isReflection xs i
    | i < 1 || i >= length xs = False
    | otherwise = smudge 1 t
        where
            c = min (length xs - i) i
            t = [(xs !! (i-j), xs !! (i+j-1)) | j <- [1..c]]

findReflection :: [String] -> Int
findReflection = p 1
    where
        p :: Int -> [String] -> Int
        p i xs
          | i > length xs = 0 
          | isReflection xs i = i
          | otherwise = p (i+1) xs

problems :: [String] -> [[String]]
problems [] = []
problems ("":xs) = problems xs
problems xs = takeWhile (/= "") xs : problems (dropWhile (/= "") xs)

solveEach :: [String] -> Int
solveEach xs = r * 100 + c
    where
        rows = xs
        cols = transpose xs
        r = findReflection rows
        c = findReflection cols


solve :: String -> Int
solve = sum . map solveEach . problems . lines