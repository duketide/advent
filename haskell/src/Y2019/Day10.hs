module Y2019.Day10 (solve) where

import AOC (getInput)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type Point = (Int, Int)

divMod_ :: Int -> Int -> Point
divMod_ a b = (x, y)
  where
    (y, x) = divMod a b

getSlope :: Point -> Point -> Point
getSlope (x1, y1) (x2, y2) = (x, y)
  where
    x' = x2 - x1
    y' = y2 - y1
    d = max 1 $ gcd x' y'
    x = x' `div` d
    y = y' `div` d

p1List :: Int -> String -> [Point]
p1List row s = snd $ foldl (\(ind, acc) x -> if x == '#' then (ind + 1, divMod_ ind row : acc) else (ind + 1, acc)) (0, []) s

solve :: IO (Int, Int)
solve = do
  input <- getInput "2019" "10"
  let row = length $ takeWhile (/= '\n') input
      s = filter (/= '\n') input
      l = p1List row s
      r = map (\x -> nub $ getSlope x <$> l) l
      p1 = maximum (length <$> r) - 1 -- can see self
  return (p1, 0)
