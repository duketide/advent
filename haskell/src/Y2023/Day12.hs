module Y2023.Day12 (solve) where

import AOC (getInput)
import Data.List (group)
import Data.List.Split (splitOn)

lineParse :: String -> ([String], [Int])
lineParse = f . splitOn " "
  where
    f (a : b : _) = ([a], read <$> splitOn "," b)

roll :: String -> [String]
roll s
  | '?' `notElem` s = [s]
  | otherwise = [a, b]
  where
    (x, y) = span (/= '?') s
    a = x ++ ('.' : tail y)
    b = x ++ ('#' : tail y)

genStrings :: ([String], [Int]) -> ([String], [Int])
genStrings (s, n) = (go s, n)
  where
    go s
      | any (elem '?') s = go $ s >>= roll
      | otherwise = s

stringTally :: String -> [Int]
stringTally = map length . filter (all (== '#')) . group

lineScore :: ([String], [Int]) -> Int
lineScore x = ct
  where
    (s, n) = genStrings x
    tally = stringTally <$> s
    ct = length $ filter (== n) tally

p1 :: [([String], [Int])] -> Int
p1 = sum . map lineScore

solve :: IO (Int, Int)
solve = do
  input <- map lineParse . lines <$> getInput "2023" "12"
  return (p1 input, 0)
