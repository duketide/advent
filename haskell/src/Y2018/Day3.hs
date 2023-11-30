module Y2018.Day3 (solve) where

import AOC (getInput)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M

type Counter = Map (Int, Int) Bool

type Claim = (Int, Int, Int, Int)

tally :: Claim -> Counter -> Counter
tally (l, t, w, h) = go l t
  where
    go :: Int -> Int -> Counter -> Counter
    go col row mp
      | col == l + w - 1 && row == t + h - 1 = newMap
      | col == l + w - 1 = go l (row + 1) newMap
      | otherwise = go (col + 1) row newMap
      where
        newMap = M.insert (row, col) (M.member (row, col) mp) mp

p1Parse :: [String] -> Claim
p1Parse (x : y : xs) = (n1, n2, n3, n4)
  where
    tAndL = splitOn "," $ init x
    wAndH = splitOn "x" y
    (t : l : xs) = init $ splitOn "," x
    (w : h : ys) = splitOn "x" y
    (n1, n2) = case tAndL of
      (t : l : _) -> (read t, read l)
      _ -> error (show tAndL)
    (n3, n4) = case wAndH of
      (w : h : _) -> (read w, read h)
      _ -> error y

p1 :: [Claim] -> Int
p1 clms = length $ filter id $ M.elems finalMap
  where
    finalMap = foldr tally M.empty clms

solve :: IO (Int, Int)
solve = do
  input <- map words . lines <$> getInput "2018" "3"
  let p1Input = p1Parse . drop 2 <$> input
  return (p1 p1Input, 0)
