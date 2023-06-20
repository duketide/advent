module Y2019.Day18 (solve) where

import AOC (getInput)
import Data.Char (toUpper)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S

type Point = (Int, Int)

data Remove = Remove deriving (Eq, Show)

type TileCheck = Either Remove (Set Char)

data State = State
  { doors :: TileCheck,
    curr :: Point
  }
  deriving (Eq)

solve :: IO (Int, Int)
solve = do
  input <- getInput "2019" "18"
  let (mp, start) = mapFloor input
      numKeys = foldr (\x acc -> if x `elem` ['a' .. 'z'] then acc + 1 else acc) 0 $ fmap snd . M.assocs $ mp
      startState = State {doors = Right S.empty, curr = start}
  print $ length $ filter (\x -> x /= '#' && x /= '\n') input
  return (p1 startState numKeys mp, 0)

p1 :: State -> Int -> Map Point Char -> Int
p1 s numKeys mp = go 0 [s]
  where
    go n st
      | any (\(State d _) -> ((== numKeys) . length <$> d) == Right True) st = n
      | otherwise = go (n + 1) (nub $ st >>= nextStates mp)

mapFloor :: String -> (Map Point Char, Point)
mapFloor = fst . foldr f ((M.empty, (-1, -1)), (0, 0))
  where
    f tile ((m, s), (x, y)) = case tile of
      '\n' -> ((m, s), (0, y + 1))
      '@' -> ((nextMp, (x, y)), nextP)
      _ -> ((nextMp, s), nextP)
      where
        nextMp = M.insert (x, y) tile m
        nextP = (x + 1, y)

nextStates :: Map Point Char -> State -> [State]
nextStates mp (State doors c@(x, y)) = next
  where
    next = filter (\(State d t) -> d /= Left Remove) [up, down, left, right]
    isKey = tile `elem` ['a' .. 'z'] && (not . S.member (toUpper tile) <$> doors) == Right True
    tile = fromMaybe '?' (M.lookup (x, y) mp)
    up = State (tileCheck c doors mp) (x, y - 1)
    down = State (tileCheck c doors mp) (x, y + 1)
    left = State (tileCheck c doors mp) (x - 1, y)
    right = State (tileCheck c doors mp) (x + 1, y)

tileCheck :: Point -> TileCheck -> Map Point Char -> TileCheck
tileCheck pt s mp = case M.lookup pt mp of
  Nothing -> Left Remove
  Just '#' -> Left Remove
  Just x -> f x
    where
      f a
        | a `elem` ['a' .. 'z'] = S.insert (toUpper a) <$> s
        | a `elem` ['A' .. 'Z'] = if (S.member a <$> s) == Right True then s else Left Remove
        | otherwise = s
