module Y2019.Day16 (solve) where

import AOC (getInput, readInt, trim)
import Data.Char (digitToInt, intToDigit)

solve :: IO (String, String)
solve = do
  input <- fmap digitToInt . trim <$> getInput "2019" "16"
  return (p1 input, p2 input)

p1 :: [Int] -> String
p1 = go 0
  where
    go 100 l = showMsg l
    go n l = go (n + 1) $ singleMap l

showMsg :: [Int] -> String
showMsg = fmap intToDigit . take 8

singleMap :: [Int] -> [Int]
singleMap nums = genNum nums <$> [1 .. len]
  where
    len = length nums

genNum :: [Int] -> Int -> Int
genNum lst m = (`mod` 10) . abs . sum $ f
  where
    s = [0, 1, 0, -1] >>= replicate m
    f = uncurry (*) <$> zip lst (tail $ concat $ repeat s)

singleFold :: [Int] -> [Int]
singleFold = fst . foldr (\x (lst, n) -> let num = (x + n) `mod` 10 in (num : lst, num)) ([], 0)

p2 :: [Int] -> String
p2 inp = go 0 (drop offset $ concat $ replicate 10000 inp)
  where
    offset = readInt $ intToDigit <$> take 7 inp
    go 100 l = showMsg l
    go n l = go (n + 1) (singleFold l)
