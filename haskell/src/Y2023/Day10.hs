module Y2023.Day10 (solve) where

import AOC (getInput, nbrs, nbrs4)
import Data.List (groupBy, intersect, nub, sortBy)
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
    next = nbrs4 (x, y)
    f (x', y') = connectTest current (M.lookup (x', y') ps) (x' - x, y' - y)

p1 :: Point -> Pipes -> Int
p1 start mp = go 0 (pure start) S.empty
  where
    go c ps seen
      | not (null (ps `intersect` nextPs)) || (length (nub ps) == 1 && c > 0) = c
      | otherwise = go (c + 1) nextPs (foldr S.insert seen ps)
      where
        nextPs = filter (not . (`S.member` seen)) $ ps >>= (`nextPipes` mp)

loopFinder :: Point -> Pipes -> Set Point
loopFinder start mp = go (pure start) S.empty
  where
    go ps seen
      | not (null (ps `intersect` nextPs)) || (length (nub ps) == 1 && head ps /= start) = foldr S.insert seen ps
      | otherwise = go nextPs (foldr S.insert seen ps)
      where
        nextPs = filter (not . (`S.member` seen)) $ ps >>= (`nextPipes` mp)

p2 :: String -> Int
p2 rawInp = result
  where
    input = map (\x -> if x == 'S' then sMapper start inpMap else x) rawInp
    inpMap = parser rawInp
    start = fst $ head $ filter ((== 'S') . snd) $ M.assocs inpMap
    loop = loopFinder start inpMap
    (_, _, _, result) = foldl fn ((0, 0), firstVert, False, 0) input
      where
        firstVert = if (0, 0) `S.member` loop && isVert (head rawInp) then head rawInp else ' '
        fn ((x, y), _, _, ct) '\n' = ((0, y + 1), ' ', False, ct)
        fn ((x, y), lastVert, bool, ct) c
          | not (S.member (x, y) loop) = ((x + 1, y), lastVert, bool, if bool then ct + 1 else ct)
          | not (isVert c) = ((x + 1, y), lastVert, bool, ct)
          | toggle c lastVert = ((x + 1, y), ' ', not bool, ct)
          | lastVert == ' ' = ((x + 1, y), c, bool, ct)
          | otherwise = ((x + 1, y), ' ', bool, ct)

toggle :: Char -> Char -> Bool
toggle c l =
  c == '|'
    || (l == 'F' && c == 'J')
    || (l == 'L' && c == '7')

isVert :: Char -> Bool
isVert c = c == '|' || c == '7' || c == 'F' || c == 'L' || c == 'J'

sMapper :: Point -> Pipes -> Char
sMapper (x, y) ps = c
  where
    [u, d, l, r] = (`M.lookup` ps) <$> [(x, y - 1), (x, y + 1), (x - 1, y), (x, y + 1)]
    up = u == Just '|' || u == Just 'F' || u == Just '7'
    down = d == Just '|' || d == Just 'J' || d == Just 'L'
    left = l == Just '-' || l == Just 'L' || l == Just 'F'
    right = r == Just '-' || r == Just '7' || r == Just 'J'
    c
      | up && down = '|'
      | right && left = '-'
      | up && right = 'L'
      | up && left = 'J'
      | down && right = 'F'
      | down && left = '7'
      | otherwise = error "s-map error"

solve :: IO (Int, Int)
solve = do
  input <- getInput "2023" "10"
  let inpMap = parser input
      start = fst $ head $ filter ((== 'S') . snd) $ M.assocs inpMap
  return (p1 start inpMap, p2 input)
