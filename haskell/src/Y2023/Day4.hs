module Y2023.Day4 (solve) where

import AOC (getInput)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S

-- using sets assumes no duplicate numbers in winner list or "your numbers" list for a given card
data Card = Card
  { winners :: Set String,
    nums :: Set String
  }
  deriving (Show)

type P2State = (Int, Map Int Int, Int)

p1 :: [String] -> Int
p1 = foldr f 0 . map2wins
  where
    f wins acc = acc + if wins > 0 then 2 ^ (wins - 1) else 0

map2wins :: [String] -> [Int]
map2wins = map f . inpParse
  where
    f c = S.size $ S.intersection (winners c) (nums c)

inpParse :: [String] -> [Card]
inpParse = map (f . map words . splitOn "|")
  where
    f [_ : _ : t, ys] = Card (S.fromList t) (S.fromList ys)
    f x = error "bad parse"

p2 :: [String] -> Int
p2 inp = res
  where
    (_, _, res) = foldl p2tally (1, M.empty, 0) (map2wins inp)

p2tally :: P2State -> Int -> P2State
p2tally (idx, mults, result) wins = (idx + 1, newMults, result + currCount)
  where
    currCount = 1 + fromMaybe 0 (M.lookup idx mults)
    newMults = go mults (idx + 1)
      where
        go :: Map Int Int -> Int -> Map Int Int
        go mp i
          | i - idx > wins = mp
          | otherwise = go (M.insertWith (+) i currCount mp) (i + 1)

solve :: IO (Int, Int)
solve = do
  input <- lines <$> getInput "2023" "4"
  return (p1 input, p2 input)
