{-# LANGUAGE NamedFieldPuns #-}

module Y2023.Day5 (solve) where

import AOC (getInput, readInt)
import Data.Bifunctor (Bifunctor (first))
import Data.List.Split (chunksOf, splitOn)

data InvMap = InvMap {dest, src, rl :: Int} deriving (Show)

rangeTest :: Int -> InvMap -> (Int, Bool)
rangeTest n (InvMap {dest, src, rl}) = (if test then dest + n - src else n, test)
  where
    test = n >= src && n <= src + rl - 1

mapParse :: String -> [InvMap]
mapParse = map (f . map readInt . words) . tail . lines
  where
    f (x : y : z : _) = InvMap x y z
    f _ = error "bad InvMap parse"

conversion :: Int -> [InvMap] -> Int
conversion n [] = n
conversion n (x : xs)
  | test = newN
  | otherwise = conversion n xs
  where
    (newN, test) = rangeTest n x

p1 :: [String] -> Int
p1 (x : xs) = minimum $ foldl func seeds invMaps
  where
    func sds invMs = map (`conversion` invMs) sds
    seeds = readInt <$> tail (words x)
    invMaps = mapParse <$> xs

-- ranges uses boolean flags to track which ranges have been mapped
ranges :: (Bool, (Int, Int)) -> InvMap -> [(Bool, (Int, Int))]
ranges n@(True, _) _ = pure n
ranges (_, (s1, len)) (InvMap {dest, src = s2, rl})
  | s1 < s2 && e1 > e2 = [(True, (s2 + d, rl)), (False, (s1, s2 - s1)), (False, (e2 + 1, len - rl - (s2 - s1)))]
  | s1 >= s2 && e1 <= e2 = pure (True, (s1 + d, len))
  | e1 < s2 || s1 > e2 = pure (False, (s1, len))
  | s1 >= s2 = [(True, (s1 + d, o1)), (False, (s1 + o1, len - o1))]
  | otherwise = [(False, (s1, len - o2)), (True, (s1 + len - o2 + d, o2))]
  where
    e1 = s1 + len - 1
    e2 = s2 + rl - 1
    d = dest - s2
    o1 = e2 - s1 + 1
    o2 = e1 - s2 + 1

rangeFolder :: (Bool, (Int, Int)) -> [InvMap] -> [(Bool, (Int, Int))]
rangeFolder r = map (first (const False)) . foldl (\acc m -> acc >>= (`ranges` m)) [r]

p2 :: [String] -> Int
p2 (x : xs) = minimum $ map (fst . snd) $ foldl func seeds invMaps
  where
    func sds invMs = concatMap (`rangeFolder` invMs) sds
    seeds = map (\(x : y : _) -> (False, (x, y))) $ chunksOf 2 $ readInt <$> tail (words x)
    invMaps = mapParse <$> xs

solve :: IO (Int, Int)
solve = do
  input <- splitOn "\n\n" <$> getInput "2023" "5"
  return (p1 input, p2 input)
