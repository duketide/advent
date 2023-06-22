module Y2019.Day19 (solve) where

import AOC (getInput)
import IntCom (Program, Return (outputs), execute, program)

solve :: IO (Int, Int)
solve = do
  input <- program <$> getInput "2019" "19"
  return (p1 input, p2 input)

p1 :: Program -> Int
p1 p = foldr (\(x, y) acc -> let (u : _) = outputs (execute 0 [x, y] 0 p) in u + acc) 0 lst
  where
    lst = [(x, y) | x <- [0 .. 49], y <- [0 .. 49]]

p2 :: Program -> Int
p2 p = go 0 row
  where
    row = binSearch p
    go x y
      | win = x * 10000 + y
      | otherwise = go (x + 1) y
      where
        hit = 1 == (head . outputs $ execute 0 [x, y] 0 p)
        down = 1 == (head . outputs $ execute 0 [x, y + 99] 0 p)
        right = 1 == (head . outputs $ execute 0 [x + 99, y] 0 p)
        diag = 1 == (head . outputs $ execute 0 [x + 99, y + 99] 0 p)
        win = hit && down && right && diag

bounds :: Program -> (Int, Int)
bounds p = go 1
  where
    go n
      | testRow p n = (n, n `div` 2)
      | otherwise = go (n * 2)

binSearch :: Program -> Int
binSearch p = go lo hi
  where
    (hi, lo) = bounds p
    go l h
      | testRow p mid && not others = mid
      | testRow p mid || others = go l mid
      | otherwise = go mid h
      where
        others = or (testRow p <$> [mid - 10 .. mid - 1])
        mid = (l + h) `div` 2

testRow :: Program -> Int -> Bool
testRow p y = go 0 y False
  where
    go x y b
      | win = True
      | b && not hit = False
      | hit && not b = go (x + 1) y True
      | x == 2 * y && not b = False
      | otherwise = go (x + 1) y b
      where
        hit = 1 == (head . outputs $ execute 0 [x, y] 0 p)
        down = 1 == (head . outputs $ execute 0 [x, y + 99] 0 p)
        right = 1 == (head . outputs $ execute 0 [x + 99, y] 0 p)
        diag = 1 == (head . outputs $ execute 0 [x + 99, y + 99] 0 p)
        win = hit && down && right && diag
