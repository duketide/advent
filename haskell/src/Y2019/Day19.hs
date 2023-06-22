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
p2 p = go 0 99
  where
    go x y
      | here && there = x * 10000 + y - 99
      | here = go x (y + 1)
      | otherwise = go (x + 1) y
      where
        here = 1 == (head . outputs $ execute 0 [x, y] 0 p)
        there = 1 == (head . outputs $ execute 0 [x + 99, y - 99] 0 p)
