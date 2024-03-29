module Y2023.Day4 (solve) where

import AOC (getInput)
import Data.List (intersect)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

type P2State = (Int, Map Int Int, Int)

p1 :: [String] -> Int
p1 = foldr f 0 . inpParse
  where
    f wins acc = acc + if wins > 0 then 2 ^ (wins - 1) else 0

inpParse :: [String] -> [Int]
inpParse = map (f . map words . splitOn "|")
  where
    f [_ : _ : t, ys] = length $ t `intersect` ys
    f x = error "bad parse"

p2 :: [String] -> Int
p2 inp = res
  where
    (_, _, res) = foldl p2tally (1, M.empty, 0) (inpParse inp)

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
