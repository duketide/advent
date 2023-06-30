module Y2019.Day22 (solve) where

import AOC (getInput, readInt)
import Data.Bits (shiftR)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type Pair = (Integer, Integer)

p1Length = 10007

p2Length = 119315717514047

p2Cycles = 101741582076661

solve :: IO (Integer, Integer)
solve = do
  input <- fmap words . lines <$> getInput "2019" "22"
  let (offsetDiff, incrementMult) = singleTurn p2Length input
      -- increment for a number of iterations = incrementMult^iterations mod length
      -- offset for a number of iterations = prevOffset + (prevIncrement * offsetDiff)
      -- so final increment can be calculated with exponentiation, final offset with sum of geometric series
      finalInc = modExp incrementMult p2Cycles p2Length
      -- final offset is sum of finite geometric series, see Wikipedia page ((a(1-r^n))/(1-r)), a == offsetDiff, r == incrementMult
      finalOffset = (`mod` p2Length) $ offsetDiff * (1 - modExp incrementMult p2Cycles p2Length) * modExp (1 - incrementMult) (p2Length - 2) p2Length
      p2 = (finalOffset + finalInc * 2020) `mod` p2Length
  return (p1 input, p2)

p1 :: [[String]] -> Integer
p1 = foldl f 2019
  where
    f acc i@(x : y : _)
      | x == "cut" = cut (read y) acc p1Length
      | y == "into" = p1Length - 1 - acc
      | otherwise = increment (read $ i !! 3) acc p1Length

cut :: Integer -> Integer -> Integer -> Integer
cut ct ind len
  | ct == 0 = ind
  | otherwise = (ind - ct) `mod` len

increment :: Integer -> Integer -> Integer -> Integer
increment inc ind len = ind * inc `mod` len

cut' :: Pair -> Integer -> Integer -> Integer
cut' (offset, inc) n len = (offset + inc * n) `mod` len

reverse' :: Pair -> Integer -> Pair
reverse' (offset, inc) len = ((offset - inc) `mod` len, -inc `mod` len)

increment' :: Pair -> Integer -> Integer -> Integer
increment' (offset, inc) n len = inc * modExp n (len - 2) len -- assumes len is prime

-- implemented based on pseduocode on Wikipedia page for modular exponentiation
modExp :: Integer -> Integer -> Integer -> Integer
modExp b p 1 = 0
modExp b p m = go b p 1
  where
    go base pow r
      | pow <= 0 = r
      | otherwise = go b' p' r'
      where
        b' = base ^ 2 `mod` m
        r' = if odd pow then (r * base) `mod` m else r
        p' = shiftR pow 1

singleTurn :: Integer -> [[String]] -> Pair
singleTurn len = foldl f (0, 1)
  where
    f acc@(offset, inc) i@(x : y : _)
      | x == "cut" = (cut' acc (read y) len, inc)
      | y == "into" = reverse' acc len
      | otherwise = (offset, increment' acc (read $ i !! 3) len)
