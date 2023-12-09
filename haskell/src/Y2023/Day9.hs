module Y2023.Day9 (solve) where

import AOC (getInput, readInt)
import Data.List (nub)

nextVal :: [Int] -> [Int] -> Int
nextVal xs ns
  | length (nub ns) == 1 = head ns + sum xs
  | otherwise = nextVal (last ns : xs) (nextDiffs ns)

nextDiffs :: [Int] -> [Int]
nextDiffs ns = zipWith (-) (tail ns) ns

p1 :: [[Int]] -> Int
p1 = sum . map (nextVal [])

prevVal :: [Int] -> [Int] -> Int
prevVal xs ns
  | length (nub ns) == 1 = foldl (flip (-)) (head ns) xs
  | otherwise = prevVal (head ns : xs) (nextDiffs ns)

p2 :: [[Int]] -> Int
p2 = sum . map (prevVal [])

solve :: IO (Int, Int)
solve = do
  input <- map (map readInt . words) . lines <$> getInput "2023" "9"
  return (p1 input, p2 input)
