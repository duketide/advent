module Y2023.Day12 (solve) where

import AOC (getInput)
import Data.List (group)
import Data.List.Split (splitOn)

init' :: [a] -> [a]
init' [] = []
init' x = init x

last' :: [Char] -> Char
last' [] = ' '
last' x = last x

lineParse' :: String -> (String, [Int])
lineParse' = f . splitOn " "
  where
    f (a : b : _) = (a, read <$> splitOn "," b)

roll :: String -> [String]
roll s
  | '?' `notElem` s = [s]
  | otherwise = [a, b]
  where
    (x, y) = span (/= '?') s
    a = x ++ ('.' : tail y)
    b = x ++ ('#' : tail y)

stringTally :: String -> [Int]
stringTally = map length . filter (all (== '#')) . group

partialScore :: String -> [Int]
partialScore s = (if last' r == '.' then id else init') $ stringTally r
  where
    r = takeWhile (/= '?') s

match :: [Int] -> [Int] -> Bool
match a b = all (uncurry (==)) $ zip a b

lineGen :: String -> [Int] -> Int
lineGen s ints = go [s] n
  where
    n = length $ filter (== '?') s
    go :: [String] -> Int -> Int
    go strs 0 = length $ filter ((== ints) . stringTally) strs
    go strs x = go nextStrs (x - 1)
      where
        nextStrs = filter (match ints . partialScore) strs >>= roll

p1 :: [(String, [Int])] -> Int
p1 = sum . map (uncurry lineGen)

p2InputMapper :: (String, [Int]) -> (String, [Int])
p2InputMapper (s, ints) = (concat (replicate 4 s') ++ s, concat (replicate 5 ints))
  where
    s' = s ++ "?"

solve :: IO (Int, Int)
solve = do
  input <- map lineParse' . lines <$> getInput "2023" "12"
  print $ p1 input
  return (p1 input, p1 $ p2InputMapper <$> input)
