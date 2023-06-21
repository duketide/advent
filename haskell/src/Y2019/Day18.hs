module Y2019.Day18 (solve) where

import AOC (getInput)
import Data.Char (isLower, toUpper)
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as H
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set)
import qualified Data.Set as S

type Point = (Int, Int)

data State = State
  { doors :: Set Char,
    curr :: Point,
    turn :: Int
  }
  deriving (Eq, Ord, Show)

type StateMemo = Map Point (Map (Set Char) Int)

solve :: IO (Int, Int)
solve = do
  input <- getInput "2019" "18"
  let (mp, start) = mapFloor input
      numKeys = foldr (\x acc -> if x `elem` ['a' .. 'z'] then acc + 1 else acc) 0 $ fmap snd . M.assocs $ mp
      startState = State {doors = S.empty, curr = start, turn = 0}
  return (p1 mp startState, 0)

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

p1 :: Map Point Char -> State -> Int
p1 mp st = go (H.singleton (0, st)) 0 M.empty
  where
    go :: MinPrioHeap Int State -> Int -> StateMemo -> Int
    go hp n sm
      | d >= 52 = pr
      | otherwise = go hp1 (n + 1) sm'
      where
        ((pr, pt), hp0) = fromMaybe (error "empty heap") $ H.view hp
        d = length $ doors pt
        nextS = S.filter (\(State d' c' t') -> t' < fromMaybe (10 ^ 9) (M.lookup c' sm >>= M.lookup d')) $ bfsGrowth mp pt
        (hp1, sm') = foldr (\x (h, m) -> (H.insert (turn x, x) h, f x m)) (hp0, sm) nextS
          where
            f st smem = M.insert (curr st) next smem
              where
                next = M.insert (doors st) (turn st) $ fromMaybe M.empty $ M.lookup (curr st) smem

bfsGrowth :: Map Point Char -> State -> Set State
bfsGrowth mp c@(State d (x, y) t) = go [c] S.empty S.empty 0
  where
    go [] _ sts _ = sts
    go locs seen sts tn = go nLocs nSeen nSts (tn + 1)
      where
        nSeen = S.union seen (S.fromList . fmap curr $ locs)
        nLocs' = locs' >>= growState mp seen
        nLocs = nub nLocs'
        -- this locs' trick (don't go through a key to get to another key) cut run time from ~7 minutes to < 10 seconds
        locs' = filter (\x -> let t = fromMaybe '?' (M.lookup (curr x) mp) in not (t `notElem` doors x && isLower t)) locs
        nSts = S.union sts $ foldr g S.empty locs
        g (State d cr t) acc = if tile cr `elem` ['a' .. 'z'] && not (S.member (tile cr) d) then S.insert (State (S.insert (toUpper (tile cr)) $ S.insert (tile cr) d) cr t) acc else acc
          where
            tile cr = fromMaybe '?' $ M.lookup cr mp

growState :: Map Point Char -> Set Point -> State -> [State]
growState mp seen (State d (x, y) t) = next
  where
    up = State d (x, y - 1) (t + 1)
    down = State d (x, y + 1) (t + 1)
    left = State d (x - 1, y) (t + 1)
    right = State d (x + 1, y) (t + 1)
    next = filter (tileCheck seen mp) [up, down, left, right]

tileCheck :: Set Point -> Map Point Char -> State -> Bool
tileCheck seen mp (State doors pt@(x, y) _)
  | S.member pt seen = False
  | tile == '#' = False
  | tile `elem` ['A' .. 'Z'] && not (S.member tile doors) = False
  | otherwise = True
  where
    tile = fromMaybe '#' $ M.lookup pt mp
