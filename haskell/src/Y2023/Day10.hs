module Y2023.Day10 (solve) where

import AOC (getInput, nbrs)
import Data.List (intersect, nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type Point = (Int, Int)

type Pipes = Map Point Char

parser :: String -> Pipes
parser = fst . foldl f (M.empty, (0, 0))
  where
    f (m, (x, y)) c
      | c == '\n' = (m, (0, y + 1))
      | otherwise = (M.insert (x, y) c m, (x + 1, y))

goesNorth :: Char -> Bool
goesNorth c = c == '|' || c == 'J' || c == 'L' || c == 'S'

goesSouth :: Char -> Bool
goesSouth c = c == '|' || c == '7' || c == 'F' || c == 'S'

goesEast :: Char -> Bool
goesEast c = c == '-' || c == 'L' || c == 'F' || c == 'S'

goesWest :: Char -> Bool
goesWest c = c == '-' || c == 'J' || c == '7' || c == 'S'

connects :: Char -> Char -> (Int, Int) -> Bool
connects x y d =
  (goesEast x && goesWest y && d == (1, 0))
    || (goesWest x && goesEast y && d == (-1, 0))
    || (goesSouth x && goesNorth y && d == (0, 1))
    || (goesNorth x && goesSouth y && d == (0, -1))

connectTest :: Char -> Maybe Char -> (Int, Int) -> Bool
connectTest c n d = case n of
  Nothing -> False
  Just x -> connects c x d

nextPipes :: Point -> Pipes -> [Point]
nextPipes (x, y) ps = filter f next
  where
    current = ps M.! (x, y)
    next = nbrs (x, y)
    f (x', y') = connectTest current (M.lookup (x', y') ps) (x' - x, y' - y)

p1 :: Point -> Pipes -> Int
p1 start mp = go 0 (pure start) S.empty
  where
    go c ps seen
      | not (null (ps `intersect` nextPs)) || (length (nub ps) == 1 && c > 0) = c
      | otherwise = go (c + 1) nextPs (foldr S.insert seen ps)
      where
        nextPs = filter (not . (`S.member` seen)) $ ps >>= (`nextPipes` mp)

solve :: IO (Int, Int)
solve = do
  input <- parser <$> getInput "2023" "10"
  let start = fst $ head $ filter ((== 'S') . snd) $ M.assocs input
  return (p1 start input, 0)
