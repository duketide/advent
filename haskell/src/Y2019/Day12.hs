module Y2019.Day12 (solve) where

import AOC (getInput)
import Data.Char (isDigit)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as S

type Vec = (Int, Int, Int)

type Moon = (Vec, Vec)

type Pair = (Int, Int)

gravity :: [Moon] -> [Moon]
gravity o = foldr update [] o
  where
    update n@(pos@(p1, p2, p3), (x, y, z)) acc = (pos, (x + dx, y + dy, z + dz)) : acc
      where
        (dx, dy, dz) = foldr (\m@((a, b, c), _) (dx, dy, dz) -> if m == n then (dx, dy, dz) else (f p1 a dx, f p2 b dy, f p3 c dz)) (0, 0, 0) o
          where
            f i j d = case i `compare` j of
              GT -> d - 1
              LT -> d + 1
              EQ -> d

velocity :: [Moon] -> [Moon]
velocity = fmap (\((x, y, z), (a, b, c)) -> ((x + a, y + b, z + c), (a, b, c)))

energy :: [Moon] -> Int
energy = sum . fmap (\((x, y, z), (a, b, c)) -> (abs x + abs y + abs z) * (abs a + abs b + abs c))

turns :: Int -> [Moon] -> Int
turns 0 m = energy m
turns n m = turns (n - 1) (velocity . gravity $ m)

singleGrav :: [Pair] -> [Pair]
singleGrav o = foldr update [] o
  where
    update (p, v) acc = (p, v + dv) : acc
      where
        dv = foldr (\(a, b) d -> f p a d) 0 o
          where
            f i j dx = case i `compare` j of
              GT -> dx - 1
              LT -> dx + 1
              EQ -> dx

singleVel :: [Pair] -> [Pair]
singleVel = fmap (\(x, y) -> (x + y, y))

singleTurn :: [Pair] -> [Pair]
singleTurn = singleVel . singleGrav

cycleFinder :: [Pair] -> Int
cycleFinder ps = go 1 (S.singleton ps) ps
  where
    go n s pairs = if S.member next s then n else go (n + 1) (S.insert next s) next
      where
        next = singleTurn pairs

solve :: IO (Int, Int)
solve = do
  input <- parse <$> getInput "2019" "12"
  let xs = fmap (\((x, _, _), _) -> (x, 0)) input
      ys = fmap (\((_, y, _), _) -> (y, 0)) input
      zs = fmap (\((_, _, z), _) -> (z, 0)) input
      p2 = foldr lcm 1 $ cycleFinder <$> [xs, ys, zs]
  return (turns 1000 input, p2)

parse :: String -> [Moon]
parse s = moonify . map (filter isDigOrNeg) . splitOn "," <$> lines s
  where
    moonify (x : y : z : _) = ((read x, read y, read z), (0, 0, 0))

isDigOrNeg :: Char -> Bool
isDigOrNeg c = isDigit c || c == '-'
