module Y2019.Day10 (solve) where

import AOC (getInput)
import Data.List (delete, maximumBy, nub, sortBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as S

type Point = (Int, Int)

data Quadrant = I | II | III | IV deriving (Eq, Ord, Show)

pointCompare :: Point -> Point -> Ordering
pointCompare a b = byQuad <> slope
  where
    byQuad = getQuadrant a `compare` getQuadrant b
    quad = getQuadrant a
    ax' = realToFrac $ fst a
    bx' = realToFrac $ fst b
    ay' = realToFrac $ snd a
    by' = realToFrac $ snd b
    slopeA = if fst a == 0 then 10000000 else ay' / ax'
    slopeB = if fst b == 0 then 10000000 else by' / bx'
    slope = slopeA `compare` slopeB

getQuadrant :: Point -> Quadrant
getQuadrant (x, y)
  | x >= 0 && y < 0 = I
  | x > 0 && y >= 0 = II
  | x <= 0 && y > 0 = III
  | otherwise = IV

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

find200 :: Set Point -> Point -> Point -> Int
find200 s (dx, dy) (sX, sY) = go 1
  where
    go mult
      | S.member p s = fst p * 100 + snd p
      | otherwise = go (mult + 1)
      where
        p = (dx * mult + sX, dy * mult + sY)

solve :: IO (Int, Int)
solve = do
  input <- getInput "2019" "10"
  let row = length $ takeWhile (/= '\n') input
      s = filter (/= '\n') input
      l = p1List row s
      r = map (\x -> (x, nub $ getSlope x <$> l)) l
      (station, seen) = maximumBy (comparing (length . snd)) r
      p1 = length seen - 1 -- can see self
      p2List = sortBy pointCompare $ delete (0, 0) seen
      p2Slope = p2List !! 199 -- cheated a little by checking that no more than one rotation would be required
  return (p1, find200 (S.fromList l) p2Slope station)
