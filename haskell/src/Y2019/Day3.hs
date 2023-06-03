module Y2019.Day3 (solve) where

import AOC (getInput, readInt)
import Data.List (union)
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

type Point = (Int, Int)

createSet :: [(Char, Int)] -> Set Point
createSet lst = fst $ foldl f (S.empty, (0, 0)) lst
  where
    f (s, (x, y)) (dir, num) = next
      where
        next = (nextSet, (nextX, nextY))
        right = dir == 'R'
        left = dir == 'L'
        up = dir == 'U'
        down = dir == 'D'
        nextX
          | up || down = x
          | right = x + num
          | otherwise = x - num
        nextY
          | right || left = y
          | up = y + num
          | down = y - num
        nextSet = S.union s $ S.fromList [(x', y') | x' <- [min x nextX .. max x nextX], y' <- [min y nextY .. max y nextY]]

createMap :: Set Point -> [(Char, Int)] -> Map Point Int
createMap s lst = fst $ foldl f (M.empty, (0, 0, 0)) lst
  where
    f (mp, (x, y, step)) (dir, num) = next
      where
        next = (nextMap, (nextX, nextY, nextStep))
        nextStep = step + num
        right = dir == 'R'
        left = dir == 'L'
        up = dir == 'U'
        down = dir == 'D'
        nextX
          | up || down = x
          | right = x + num
          | otherwise = x - num
        nextY
          | right || left = y
          | up = y + num
          | down = y - num
        nextMap = createMap' s mp $ zip [(x', y') | x' <- xSort [min x nextX .. max x nextX], y' <- ySort [min y nextY .. max y nextY]] [step .. nextStep - 1]
          where
            xSort = if left then reverse else id
            ySort = if down then reverse else id

createMap' :: Set Point -> Map Point Int -> [(Point, Int)] -> Map Point Int
createMap' s = foldl (\mp (pt, z) -> if S.member pt s then M.insertWith const pt z mp else mp)

solve :: IO (Int, Int)
solve = do
  rawInput <- getInput "2019" "3"
  let wire1 : wire2 : _ = map (map (\(x : xs) -> (x, readInt xs)) . splitOn ",") $ lines rawInput
      w1 = createSet wire1
      w2 = createSet wire2
      crossings = S.delete (0, 0) $ S.intersection w1 w2
      p1 = minimum $ S.toList $ S.map (\(x, y) -> abs x + abs y) crossings
      w1' = createMap crossings wire1
      w2' = createMap crossings wire2
      p2 = minimum $ S.map (\x -> w1' M.! x + w2' M.! x) crossings
  return (p1, p2)
