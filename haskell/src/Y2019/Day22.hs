module Y2019.Day22 (solve) where

import AOC (getInput, readInt)
import Data.Map (Map)
import qualified Data.Map as M

solve :: IO (Int, Int)
solve = do
  input <- fmap words . lines <$> getInput "2019" "22"
  return (p1 input, 0)

p1 :: [[String]] -> Int
p1 inp = fst $ foldr (\x (ind, win) -> if x == 2019 then (ind, True) else (if win then ind else ind - 1, win)) (10006, False) $ runP1 inp

runP1 :: [[String]] -> [Int]
runP1 = foldl f [0 .. 10006]
  where
    f acc i@(x : y : _)
      | x == "cut" = cut (readInt y) acc
      | y == "into" = reverse acc
      | otherwise = increment (readInt $ i !! 3) acc

runP1' :: [[String]] -> [Int]
runP1' = foldr f [0 .. 10006]
  where
    f i@(x : y : _) acc
      | x == "cut" = cut (readInt y) acc
      | y == "into" = reverse acc
      | otherwise = increment (readInt $ i !! 3) acc

cut :: Int -> [Int] -> [Int]
cut n lst
  | n == 0 = lst
  | n > 0 = let (x, y) = splitAt n lst in y ++ x
  | n < 0 = let (x, y) = splitAt (length lst + n) lst in y ++ x

increment :: Int -> [Int] -> [Int]
increment n lst = M.elems . snd $ foldl (\(inc, mp) x -> ((inc + n) `mod` length lst, M.insert inc x mp)) (0, M.empty) lst
