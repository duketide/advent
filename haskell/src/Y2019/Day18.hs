module Y2019.Day18 (solve) where

import AOC (getInput)
import Data.Char (isLower, isUpper, toUpper)
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

data State2 = State2
  { one :: State,
    two :: State,
    three :: State,
    four :: State
  }
  deriving (Eq, Ord, Show)

type StateMemo2 = Map State2 Int

solve :: IO (Int, Int)
solve = do
  input <- getInput "2019" "18"
  let (mp, start@(x0, y0)) = mapFloor input
      numKeys = foldr (\x acc -> if isLower x then acc + 1 else acc) 0 $ fmap snd . M.assocs $ mp
      startState = State {doors = S.empty, curr = start, turn = 0}
      input2 = mapP2 mp start
      startStates2 =
        State2
          startState {curr = (x0 - 1, y0 - 1)}
          startState {curr = (x0 + 1, y0 - 1)}
          startState {curr = (x0 + 1, y0 + 1)}
          startState {curr = (x0 - 1, y0 + 1)}
  return (p1 mp startState numKeys, p2 input2 startStates2 numKeys)

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

mapP2 :: Map Point Char -> Point -> Map Point Char
mapP2 mp (x, y) = next
  where
    lst = [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y), (x, y)]
    next = foldr (`M.insert` '#') mp lst

p1 :: Map Point Char -> State -> Int -> Int
p1 mp st nk = go (H.singleton (0, st)) M.empty
  where
    go :: MinPrioHeap Int State -> StateMemo -> Int
    go hp sm
      | d >= 2 * nk = pr
      | otherwise = go hp1 sm'
      where
        ((pr, pt), hp0) = fromMaybe (error "empty heap") $ H.view hp
        d = length $ doors pt
        nextS = S.filter (\(State d' c' t') -> t' < fromMaybe (10 ^ 9) (M.lookup c' sm >>= M.lookup d')) $ bfsGrowth mp pt
        (hp1, sm') = foldr (\x (h, m) -> (H.insert (turn x, x) h, f x m)) (hp0, sm) nextS
          where
            f st smem = M.insert (curr st) next smem
              where
                next = M.insert (doors st) (turn st) $ fromMaybe M.empty $ M.lookup (curr st) smem

p2 :: Map Point Char -> State2 -> Int -> Int
p2 mp st nk = go (H.singleton (0, st)) 0 M.empty
  where
    go :: MinPrioHeap Int State2 -> Int -> StateMemo2 -> Int
    go hp n sm
      | d >= 2 * nk = pr
      | otherwise = go hp1 (n + 1) sm'
      where
        ((pr, pt), hp0) = fromMaybe (error "empty heap") $ H.view hp
        d = length $ doors $ one pt
        nextOnes = p2Growth pt 1 mp
        nextTwos = p2Growth pt 2 mp
        nextThrees = p2Growth pt 3 mp
        nextFours = p2Growth pt 4 mp
        nextS' = foldr S.union nextOnes [nextTwos, nextThrees, nextFours]
        nextS = S.filter (\x -> turn (one x) < fromMaybe (10 ^ 9) (M.lookup (zeroOut x) sm)) nextS'
        (hp1, sm') = foldr (\x (h, m) -> (H.insert (turn $ one x, x) h, f x m)) (hp0, sm) nextS
          where
            f st = M.insert (zeroOut st) (turn $ one st)

p2Growth :: State2 -> Int -> Map Point Char -> Set State2
p2Growth s2 n mp = nextS
  where
    (State2 (State _ c1 _) (State _ c2 _) (State _ c3 _) (State _ c4 _)) = s2
    chng = case n of
      1 -> one s2
      2 -> two s2
      3 -> three s2
      4 -> four s2
      _ -> error "problem in p2Growth"
    changedStates = bfsGrowth mp chng
    nextS = foldr g S.empty changedStates
      where
        g st = S.insert (f st)
        f st = case n of
          1 -> s2 {one = st, two = st {curr = c2}, three = st {curr = c3}, four = st {curr = c4}}
          2 -> s2 {one = st {curr = c1}, two = st, three = st {curr = c3}, four = st {curr = c4}}
          3 -> s2 {one = st {curr = c1}, two = st {curr = c2}, three = st, four = st {curr = c4}}
          4 -> s2 {one = st {curr = c1}, two = st {curr = c2}, three = st {curr = c3}, four = st}

zeroOut :: State2 -> State2
zeroOut (State2 o t th fr) = State2 {one = o {turn = 0}, two = t {turn = 0}, three = th {turn = 0}, four = fr {turn = 0}}

bfsGrowth :: Map Point Char -> State -> Set State
bfsGrowth mp c@(State d (x, y) t) = go [c] S.empty S.empty 0
  where
    go [] _ sts _ = sts
    go locs seen sts tn = go nLocs nSeen nSts (tn + 1)
      where
        nSeen = S.union seen (S.fromList . fmap curr $ locs)
        nLocs' = locs' >>= growState mp seen
        nLocs = nub nLocs'
        locs' = filter (\x -> let t = fromMaybe '?' (M.lookup (curr x) mp) in not (t `notElem` doors x && isLower t)) locs
        nSts = S.union sts $ foldr g S.empty locs
        g (State d cr t) acc = if isLower (tile cr) && not (S.member (tile cr) d) then S.insert (State (S.insert (toUpper (tile cr)) $ S.insert (tile cr) d) cr t) acc else acc
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
  | isUpper tile && not (S.member tile doors) = False
  | otherwise = True
  where
    tile = fromMaybe '#' $ M.lookup pt mp
