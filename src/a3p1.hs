import Data.Char (isDigit)

main :: IO ()
main = do
  readFile "input/a3p1.txt" >>= print . solve

-- type Part position (partId, line, start, end) or PartSymbol (start, end)
type PartId = Int

type Position = (Int, Int, Int)

data Slot = PartSlot PartId Position | SymbolSlot Position deriving (Show)

readSlots :: Int -> Int -> String -> [Slot]
readSlots _ _ "" = []
readSlots l c ('.' : xs) = readSlots l (c + 1) xs
readSlots l c xs =
  if not (null partId)
    then PartSlot (read partId :: Int) (l, c, c + length partId) : readSlots l (c + length partId) rest
    else SymbolSlot (l, c, c + 1) : readSlots l (c + 1) (tail xs)
  where
    (partId, rest) = span isDigit xs

readAllSlots :: [String] -> [Slot]
readAllSlots xs = concatMap (\(lnum, line) -> readSlots lnum 1 line) $ zip [1 ..] xs

symbolPositions :: [Slot] -> [Position]
symbolPositions [] = []
symbolPositions (SymbolSlot pos : xs) = pos : symbolPositions xs
symbolPositions (_ : xs) = symbolPositions xs

isAdjacent :: Position -> Position -> Bool
isAdjacent (l1, s1, e1) (l2, s2, e2)
  | l1 == l2 = s1 == e2 || e1 == s2
  | abs (l1 - l2) == 1 = max s1 s2 <= min e1 e2
  | otherwise = False

-- extractParts :: [Symbol positions] -> [Slots] -> [PartId]
extractPartsInternal :: [Position] -> [Slot] -> [PartId]
extractPartsInternal _ [] = []
extractPartsInternal ss (PartSlot partId pos : xs)
  | any (isAdjacent pos) ss = partId : extractPartsInternal ss xs
  | otherwise = extractPartsInternal ss xs
extractPartsInternal ss (_ : xs) = extractPartsInternal ss xs

extractParts :: [Slot] -> [PartId]
extractParts slots = extractPartsInternal (symbolPositions slots) slots

solve :: String -> Int
solve = sum . extractParts . readAllSlots . lines
