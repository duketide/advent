module Y2023.Day6 (solve) where

import AOC (getInput)
import Data.Char (isDigit)

parseInp :: String -> [(Int, Int)]
parseInp = f . map (map read . tail . words) . lines
  where
    f (x : y : xs) = zip x y
    f _ = error "parse error"

tally :: (Int, Int) -> Int
tally (t, d) = go 1 (t - 1) 0
  where
    go _ 0 w = w
    go b t w = go (b + 1) (t - 1) (if b * t > d then w + 1 else w)

p1 :: String -> Int
p1 = product . map tally . parseInp

parseInp2 :: String -> (Int, Int)
parseInp2 = f . map (filter isDigit . tail) . lines
  where
    f (x : y : xs) = (read x, read y)
    f _ = error "parse2 error"

p2 :: String -> Int
p2 = tally . parseInp2

solve :: IO (Int, Int)
solve = do
  input <- getInput "2023" "6"
  return (p1 input, p2 input)
