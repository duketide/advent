module Y2018.Day3 (solve) where

import AOC (getInput)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type Counter = Map (Int, Int) Bool

type Counter2 = Map (Int, Int) Int

type Claim = (Int, Int, Int, Int)

type Claim2 = (Int, Int, Int, Int, Int)

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

p2Map :: Claim2 -> (Counter2, Set Int) -> (Counter2, Set Int)
p2Map (cl, l, t, w, h) (c2, st) = go l t c2 (S.insert cl st)
  where
    go :: Int -> Int -> Counter2 -> Set Int -> (Counter2, Set Int)
    go col row mp s
      | col == l + w - 1 && row == t + h - 1 = (newMap, newSet)
      | col == l + w - 1 = go l (row + 1) newMap newSet
      | otherwise = go (col + 1) row newMap newSet
      where
        newMap = M.insert (row, col) cl mp
        newSet = case M.lookup (row, col) mp of
          Just x -> S.delete cl $ S.delete x s
          Nothing -> s

p2Parse :: [String] -> Claim2
p2Parse (z : x : y : xs) = (claim, n1, n2, n3, n4)
  where
    claim = read $ tail z
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

p2 :: [Claim2] -> Int
p2 clms = head $ S.toList finalSet
  where
    (_, finalSet) = foldr p2Map (M.empty, S.empty) clms

solve :: IO (Int, Int)
solve = do
  input <- map words . lines <$> getInput "2018" "3"
  let p1Input = p1Parse . drop 2 <$> input
      p2Input = p2Parse . (\(x : y : xs) -> x : xs) <$> input
  return (p1 p1Input, p2 p2Input)
