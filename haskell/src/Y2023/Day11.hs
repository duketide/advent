module Y2023.Day11 (solve) where

import AOC (getInput)
import Data.List (intersect, tails, transpose)

type Point = (Int, Int)

expander :: [String] -> [String]
expander = f . transpose . f . transpose
  where
    f = concatMap (\x -> if all (== '.') x then [x, x] else [x])

galaxies :: String -> [Point]
galaxies s = res
  where
    (_, res) = foldl f ((0, 0), []) s
    f (pt@(x, y), l) c = case c of
      '\n' -> ((0, y + 1), l)
      '#' -> ((x + 1, y), pt : l)
      _ -> ((x + 1, y), l)

p1 :: String -> Int
p1 s = (`div` 2) $ sum [abs (x2 - x1) + abs (y2 - y1) | (x2, y2) <- pts, (x1, y1) <- pts, (x2, y2) /= (x1, y1)]
  where
    pts = galaxies s

expander' :: [String] -> ([Int], [Int])
expander' s = (exCols, exRows)
  where
    rows = zip [0 ..] s
    cols = zip [0 ..] $ transpose s
    exRows = foldr f [] rows
    exCols = foldr f [] cols
    f (n, s) acc = if all (== '.') s then n : acc else acc

p2 :: String -> ([Int], [Int]) -> Int
p2 s (expCols, expRows) = (`div` 2) $ sum [f a b | a <- pts, b <- pts, a /= b]
  where
    pts = galaxies s
    f (x, y) (x', y') = abs (x - x') + abs (y - y') + cols + rows
      where
        cols = 999999 * length ([min x x' .. max x x'] `intersect` expCols)
        rows = 999999 * length ([min y y' .. max y y'] `intersect` expRows)

solve :: IO (Int, Int)
solve = do
  input <- getInput "2023" "11"
  let expanse = expander' $ lines input
  return (p1 $ unlines $ expander $ lines input, p2 input expanse)
