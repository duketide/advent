module Y2023.Day9 (solve) where

import AOC (getInput, readInt)
import Data.List (nub)

nextVal :: [Int] -> [Int] -> Int
nextVal ns xs
  | length (nub ns) == 1 = head ns + sum xs
  | otherwise = nextVal (nextDiffs ns) (last ns : xs)

nextDiffs :: [Int] -> [Int]
nextDiffs ns = zipWith (-) (tail ns) ns

p1 :: [[Int]] -> Int
p1 = sum . map (`nextVal` [])

solve :: IO (Int, Int)
solve = do
  input <- map (map readInt . words) . lines <$> getInput "2023" "9"
  return (p1 input, 0)
