module Y2019.Day22 (solve) where

import AOC (getInput, readInt)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

p1Length = 10007

p2Length = 119315717514047

p2Cycles = 101741582076661

solve :: IO (Int, Int)
solve = do
  input <- fmap words . lines <$> getInput "2019" "22"
  let revP2 = backward input
      composed = foldr (.) id revP2
      (offset1, a1) = (composed 0, composed 1)
      (offset2, a2) = (composed $ composed 0, composed $ composed 1)
  print (offset1, a1 - offset1)
  print (offset2, a2 - offset2)
  return (p1 input, p2 input)

p1 :: [[String]] -> Int
p1 = foldl f 2019
  where
    f acc i@(x : y : _)
      | x == "cut" = cut (readInt y) acc p1Length
      | y == "into" = p1Length - 1 - acc
      | otherwise = increment (readInt $ i !! 3) acc p1Length

cut :: Int -> Int -> Int -> Int
cut ct ind len
  | ct == 0 = ind
  | otherwise = (ind - ct) `mod` len

increment :: Int -> Int -> Int -> Int
increment inc ind len = ind * inc `mod` len

p2 :: [[String]] -> Int
p2 inst = go times number
  where
    fs = backward inst
    (n0, n1, number) = p2Cycle fs
    cycle = n1 - n0
    times = (p2Cycles - n0) `mod` cycle
    go 0 acc = acc
    go n acc = go (n - 1) (foldr id acc fs)

p2Cycle :: [Int -> Int] -> (Int, Int, Int)
p2Cycle fs = go 2020 0 M.empty
  where
    go acc n seen
      | M.member acc seen = (seen M.! acc, n, acc)
      | otherwise = go next (n + 1) (M.insert acc n seen)
      where
        next = foldr id acc fs

forCycle :: [[String]] -> (Int, Int, Int)
forCycle inst = go 2020 0 M.empty
  where
    fs = forward inst
    go acc n seen
      | M.member acc seen = (seen M.! acc, n, acc)
      | otherwise = go next (n + 1) (M.insert acc n seen)
      where
        next = foldr id acc fs

forward :: [[String]] -> [Int -> Int]
forward = foldl f []
  where
    f acc i@(x : y : _)
      | x == "cut" = flip (cut (readInt y)) p2Length : acc
      | y == "into" = (-) (p2Length - 1) : acc
      | otherwise = flip (increment (readInt $ i !! 3)) p2Length : acc

backward :: [[String]] -> [Int -> Int]
backward = foldr f []
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
