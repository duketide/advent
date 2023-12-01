module Y2023.Day1 (solve) where

import AOC (getInput)
import Data.Char (isAlpha, isDigit)

extractDigits :: String -> Int
extractDigits s = digits
  where
    firstDig = head $ dropWhile isAlpha s
    lastDig = head $ dropWhile isAlpha $ reverse s
    digits = read [firstDig, lastDig]

p1 :: [String] -> Int
p1 = sum . map extractDigits

extractDigits2 :: String -> Int
extractDigits2 s = go s []
  where
    go :: String -> [Char] -> Int
    go x@('o' : 'n' : 'e' : _) c = go (tail x) ('1' : c)
    go x@('t' : 'w' : 'o' : _) c = go (tail x) ('2' : c)
    go x@('t' : 'h' : 'r' : 'e' : 'e' : _) c = go (tail x) ('3' : c)
    go x@('f' : 'o' : 'u' : 'r' : _) c = go (tail x) ('4' : c)
    go x@('f' : 'i' : 'v' : 'e' : _) c = go (tail x) ('5' : c)
    go x@('s' : 'i' : 'x' : _) c = go (tail x) ('6' : c)
    go x@('s' : 'e' : 'v' : 'e' : 'n' : _) c = go (tail x) ('7' : c)
    go x@('e' : 'i' : 'g' : 'h' : 't' : _) c = go (tail x) ('8' : c)
    go x@('n' : 'i' : 'n' : 'e' : _) c = go (tail x) ('9' : c)
    go (x : xs) c
      | isDigit x = go xs (x : c)
      | otherwise = go xs c
    go [] c = digits
      where
        firstDig = last c
        lastDig = head c
        digits = read [firstDig, lastDig]

p2 :: [String] -> Int
p2 = sum . map extractDigits2

solve :: IO (Int, Int)
solve = do
  input <- lines <$> getInput "2023" "1"
  return (p1 input, p2 input)
