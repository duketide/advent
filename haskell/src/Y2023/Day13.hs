module Y2023.Day13 (solve) where

import AOC (getInput)
import Data.List (transpose)
import Data.List.Split (splitOn)

test :: [String] -> Int -> Bool
test s n = all (uncurry (==)) (zip (reverse a) b)
  where
    (a, b) = splitAt n s

test2 :: [String] -> Int -> Bool
test2 s n = length nl == length nl' + 1
  where
    (a, b) = splitAt n s
    nl = zip (unlines (reverse a)) (unlines b)
    nl' = filter (uncurry (==)) nl

findReflection :: ([String] -> Int -> Bool) -> [String] -> (Bool, Int)
findReflection f = go (False, 1)
  where
    go :: (Bool, Int) -> [String] -> (Bool, Int)
    go (b, n) s
      | not b && n == length s = go (True, 1) (transpose s)
      | f s n = (b, n)
      | otherwise = go (b, n + 1) s

score :: ([String] -> Int -> Bool) -> [[String]] -> Int
score f = sum . map (g . findReflection f)
  where
    g :: (Bool, Int) -> Int
    g (b, n) = if not b then 100 * n else n

solve :: IO (Int, Int)
solve = do
  input <- map lines . splitOn "\n\n" <$> getInput "2023" "13"
  return (score test input, score test2 input)
