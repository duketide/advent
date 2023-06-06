module Y2019.Day1 (solve) where

import AOC (getInput, readInt)

reducer :: Int -> Int
reducer n = go n 0
  where
    go :: Int -> Int -> Int
    go rem res
      | rem <= 0 = res
      | otherwise = go newRem newRes
      where
        newRem = rem `div` 3 - 2
        newRes = if newRem > 0 then newRem + res else res

solve :: IO (Int, Int)
solve = do
  rawInput <- getInput "2019" "1"
  let input = map readInt $ lines rawInput
      p1 = foldr (\x acc -> x `div` 3 - 2 + acc) 0 input
      p2 = foldr (\x acc -> reducer x + acc) 0 input
  return (p1, p2)
