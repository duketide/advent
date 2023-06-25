module Y2019.Day22 (solve) where

import AOC (getInput, readInt)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

p2Length = 119315717514047

solve :: IO (Int, Int)
solve = do
  input <- fmap words . lines <$> getInput "2019" "22"
  let revP2 = p2fs input
  print $ p2' input
  return (p1 10007 input, 0)

p1 :: Int -> [[String]] -> Int
p1 len = foldl f 2019
  where
    f acc i@(x : y : _)
      | x == "cut" = cut (readInt y) acc len
      | y == "into" = len - 1 - acc
      | otherwise = increment (readInt $ i !! 3) acc len

cut :: Int -> Int -> Int -> Int
cut ct ind len
  | ct == 0 = ind
  | otherwise = (ind - ct) `mod` len

increment :: Int -> Int -> Int -> Int
increment inc ind len = ind * inc `mod` len

p2 :: Int -> Int -> [[String]] -> Int
p2 len acc input = go 0 acc
  where
    go 446491 acc = acc
    go n acc = go (n + 1) (p2iter len acc input)

p2Cycle :: Int -> [[String]] -> (Int, Int, Int)
p2Cycle len inst = go 2020 0 M.empty
  where
    go acc n seen
      | M.member acc seen = (seen M.! acc, n, acc)
      | otherwise = go next (n + 1) (M.insert acc n seen)
      where
        next = p2iter len acc inst

p2iter :: Int -> Int -> [[String]] -> Int
p2iter len = foldr f
  where
    f i@(x : y : _) acc
      | x == "cut" = rCut (readInt y) acc len
      | y == "into" = len - 1 - acc
      | otherwise = rIncrement (readInt $ i !! 3) acc len

p2' :: [[String]] -> Int
p2' inst = go 0 number
  where
    fs = p2fs inst
    (n0, n1, number) = p2Cycle' fs
    cycle = n1 - n0
    times = (p2Length - n0) `mod` cycle
    go n acc
      | n == times = acc
      | otherwise = go (n + 1) (foldr id acc fs)

p2Cycle' :: [Int -> Int] -> (Int, Int, Int)
p2Cycle' fs = go 2020 0 M.empty
  where
    go acc n seen
      | M.member acc seen = (seen M.! acc, n, acc)
      | otherwise = go next (n + 1) (M.insert acc n seen)
      where
        next = foldr id acc fs

p2fs :: [[String]] -> [Int -> Int]
p2fs = foldr f []
  where
    f i@(x : y : _) acc
      | x == "cut" = flip (rCut (readInt y)) p2Length : acc
      | y == "into" = (-) (p2Length - 1) : acc
      | otherwise = flip (rIncrement (readInt $ i !! 3)) p2Length : acc

rCut :: Int -> Int -> Int -> Int
rCut ct ind len
  | ct == 0 = ind
  | otherwise = (ind + ct) `mod` len

rIncrement :: Int -> Int -> Int -> Int
rIncrement inc ind len = ind * findReversal inc len `mod` len

findReversal :: Int -> Int -> Int
findReversal inc len = go len
  where
    go n
      | (n + 1) `mod` inc == 0 = (n + 1) `div` inc
      | otherwise = go (n + len)
