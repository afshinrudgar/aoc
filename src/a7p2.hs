module Main where

import           Data.List (sortOn)

main :: IO ()
main = do
  readFile "input/a7p2.txt" >>= print . solve

data Card = J | TWO | THREE | FOUR | FIVE | SIX | SEVEN | EIGHT | NINE | T | Q | K | A
  deriving (Show, Eq, Ord, Enum)

type Set = (Card, Card, Card, Card, Card)

data HandType = HighCard | OnePair | TwoPairs | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving (Show, Eq, Ord, Enum)

data Hand = Hand {handType :: HandType, cards :: Set} deriving (Show, Eq, Ord)

data Bid = Bid {hand :: Hand, bid :: Int} deriving (Show, Eq)

parseCard :: Char -> Card
parseCard '2' = TWO
parseCard '3' = THREE
parseCard '4' = FOUR
parseCard '5' = FIVE
parseCard '6' = SIX
parseCard '7' = SEVEN
parseCard '8' = EIGHT
parseCard '9' = NINE
parseCard 'T' = T
parseCard 'J' = J
parseCard 'Q' = Q
parseCard 'K' = K
parseCard 'A' = A
parseCard _   = error "Invalid card"

getHandType :: Set -> HandType
getHandType (c1, c2, c3, c4, c5)
  | jCnt == 5 = FiveOfAKind
  | snd (head itemCnt) + jCnt == 5 = FiveOfAKind
  | snd (head itemCnt) + jCnt == 4 = FourOfAKind
  | snd (head itemCnt) + jCnt == 3 && snd (itemCnt !! 1) == 2 = FullHouse
  | snd (head itemCnt) + jCnt == 3 = ThreeOfAKind
  | snd (head itemCnt) + jCnt == 2 && snd (itemCnt !! 1) == 2 = TwoPairs
  | snd (head itemCnt) + jCnt == 2 = OnePair
  | otherwise = HighCard
  where
    notJcnt =
      foldl
        ( \acc curr ->
            if curr == J
              then acc
              else
                if curr `elem` map fst acc
                  then
                    map
                      ( \(item, cnt) ->
                          if item == curr
                            then (item, cnt + 1)
                            else (item, cnt)
                      )
                      acc
                  else (curr, 1) : acc
        )
        []
        [c1, c2, c3, c4, c5]
    itemCnt = sortOn ((* (-1)) . snd) notJcnt
    jCnt = length (filter (== J) [c1, c2, c3, c4, c5])

parseHand :: String -> Hand
parseHand s = Hand handType' cards'
  where
    cards' = (parseCard (head s), parseCard (s !! 1), parseCard (s !! 2), parseCard (s !! 3), parseCard (s !! 4))
    handType' = getHandType cards'

parseBid :: String -> Bid
parseBid s = Bid hand' bid'
  where
    (p1, p2) = span (/= ' ') s
    hand' = parseHand p1
    bid' = read (tail p2)

solve :: String -> Int
solve s = foldl (\acc (r, b) -> acc + r * bid b) 0 (zip [1 ..] (sortOn hand bids))
  where
    bids = map parseBid (lines s)
