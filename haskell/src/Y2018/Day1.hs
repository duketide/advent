module Y2018.Day1 (solve) where

import AOC (getInput, readInt)
import Data.Set (Set)
import qualified Data.Set as S

parser :: String -> Int
parser s = case head s of
  '+' -> read $ tail s
  _ -> read s

tally :: [String] -> Int
tally = foldr folder 0
  where
    folder str acc = acc + parser str

p2 :: [String] -> Int
p2 strs = go strs 0 S.empty
  where
    go :: [String] -> Int -> Set Int -> Int
    go s c f
      | S.member c f = c
      | otherwise = go (tail s ++ [head s]) (c + parser (head s)) (S.insert c f)

solve :: IO (Int, Int)
solve = do
  input <- lines <$> getInput "2018" "1"
  return (tally input, p2 input)
