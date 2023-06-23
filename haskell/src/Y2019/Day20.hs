module Y2019.Day20 (solve) where

import AOC (getInput)
import Data.Char (isUpper)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S

type Point = (Int, Int)

solve :: IO (Int, Int)
solve = do
  input <- getInput "2019" "20"
  let portals = portalFinder input
      start = head $ portals M.! "AA"
      end = head $ portals M.! "ZZ"
      pMap = portalMap portals
      fullMap = rawMap input
  return (bfs start end fullMap pMap, 0)

bfs :: Point -> Point -> Map Point Char -> Map Point Point -> Int
bfs st end mp portals = go (S.singleton st) (S.singleton st) 0
  where
    go q seen n
      | S.member end q = n
      | otherwise = go nextQ (S.union nextQ seen) (n + 1)
      where
        nextQ' = foldr (\x acc -> S.union acc (mover x mp portals)) S.empty q
        nextQ = S.filter (\x -> not (S.member x seen)) nextQ'

mover :: Point -> Map Point Char -> Map Point Point -> Set Point
mover (x, y) mp prt = s
  where
    raw = S.fromList [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y), fromMaybe (-1, -1) (M.lookup (x, y) prt)]
    s = S.filter (\pt@(x0, y0) -> pt /= (-1, -1) && isJustDot (M.lookup pt mp)) raw

rawMap :: String -> Map Point Char
rawMap = fst . foldl (\(m, (x, y)) t -> if t == ' ' then (m, (x + 1, y)) else if t == '\n' then (m, (0, y + 1)) else (M.insert (x, y) t m, (x + 1, y))) (M.empty, (0, 0))

portalFinder :: String -> Map String [Point]
portalFinder s = foldr f M.empty (M.assocs letters)
  where
    fullMap = rawMap s
    letters = M.filter isUpper fullMap
    f ((x, y), l) mp
      | up = M.insertWith (++) val [(x, y - 1)] mp
      | down = M.insertWith (++) val [(x, y + 1)] mp
      | right = M.insertWith (++) val [(x + 1, y)] mp
      | left = M.insertWith (++) val [(x - 1, y)] mp
      | otherwise = mp
      where
        up = isJustDot $ M.lookup (x, y - 1) fullMap
        down = isJustDot $ M.lookup (x, y + 1) fullMap
        right = isJustDot $ M.lookup (x + 1, y) fullMap
        left = isJustDot $ M.lookup (x - 1, y) fullMap
        val
          | up = [l, fullMap M.! (x, y + 1)]
          | down = [fullMap M.! (x, y - 1), l]
          | right = [fullMap M.! (x - 1, y), l]
          | left = [l, fullMap M.! (x + 1, y)]

isJustDot :: Maybe Char -> Bool
isJustDot mc = mc == Just '.'

portalMap :: Map String [Point] -> Map Point Point
portalMap = foldr f M.empty
  where
    f (a : b : _) acc = M.insert b a . M.insert a b $ acc
    f _ acc = acc
