module Y2023.Day5 (solve) where

import AOC (getInput, readInt)
import Data.List.Split (splitOn)
import Language.Haskell.TH.Lens (injectivityAnnInputs)

data InvMap = InvMap {dest, src, rl :: Int} deriving (Show)

rangeTest :: Int -> InvMap -> (Int, Bool)
rangeTest n m = (if test then dest m + n - src m else n, test)
  where
    test = n >= src m && n <= src m + rl m - 1

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

solve :: IO (Int, Int)
solve = do
  input <- splitOn "\n\n" <$> getInput "2023" "5"
  return (p1 input, 0)
