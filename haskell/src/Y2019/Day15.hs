module Y2019.Day15 (solve) where

import AOC (getInput)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import IntCom (HaltOrAwait (Await), Program, Return (R, outputs, rb, state, status), execute, program)

type Point = (Int, Int)

data MapState = MapState
  { bot :: Point,
    ret :: Return
  }

type TState = (Set Point, [MapState])

type UState = ((Set Point, Map Point Int), [MapState])

initialState :: Program -> TState
initialState p =
  ( S.empty,
    return
      MapState
        { bot = (0, 0),
          ret = R {status = Await, state = (0, p), rb = 0, outputs = [1]}
        }
  )

nextStates :: MapState -> TState -> TState
nextStates MapState {bot = (x, y), ret = R {status = _, state = (ip, prog), rb = rb, outputs = outputs}} (seen, lst) = (nextSeen, nextPos)
  where
    nextPos = filter f [north, south, west, east] ++ lst
    north = MapState {bot = (x, y - 1), ret = execute ip [1] rb prog}
    south = MapState {bot = (x, y + 1), ret = execute ip [2] rb prog}
    west = MapState {bot = (x - 1, y), ret = execute ip [3] rb prog}
    east = MapState {bot = (x + 1, y), ret = execute ip [4] rb prog}
    f MapState {bot = seenCheck, ret = R {status = _, state = _, rb = _, outputs = (x : _)}} = x /= 0 && not (S.member seenCheck seen)
    nextSeen = foldr S.insert seen [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

mapper :: MapState -> UState -> UState
mapper MapState {bot = (x, y), ret = R {status = _, state = (ip, prog), rb = rb, outputs = outputs}} ((seen, mp), lst) = ((nextSeen, nextMp), nextPos)
  where
    nextPos = filter f steps ++ lst
    nextSeen = foldr S.insert seen [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]
    nextMp = foldr g mp steps
    steps = [north, south, west, east]
    north = MapState {bot = (x, y - 1), ret = execute ip [1] rb prog}
    south = MapState {bot = (x, y + 1), ret = execute ip [2] rb prog}
    west = MapState {bot = (x - 1, y), ret = execute ip [3] rb prog}
    east = MapState {bot = (x + 1, y), ret = execute ip [4] rb prog}
    f MapState {bot = seenCheck, ret = R {status = _, state = _, rb = _, outputs = (x : _)}} = x /= 0 && not (S.member seenCheck seen)
    g MapState {bot = pos, ret = R {status = _, state = _, rb = _, outputs = (x : _)}} = M.insert pos x

p1 :: TState -> Int
p1 = go 0
  where
    go n st
      | f st = n
      | otherwise = go (n + 1) $ foldr nextStates (fst st, []) (snd st)
      where
        f = any ((== 2) . head . outputs . ret) . snd

p2Map :: TState -> Map Point Int
p2Map (seen, sts) = go ((seen, M.empty), sts)
  where
    go ((_, mp), []) = mp
    go (a, b) = go $ foldr mapper (a, []) b

bfs :: Point -> Map Point Int -> Int
bfs start mp = go [start] S.empty 0
  where
    go [] _ n = n - 1
    go c s n = go nextC nextS (n + 1)
      where
        nextC = filter (\x -> not (S.member x s) && M.lookup x mp == Just 1) $ c >>= neighbors
        nextS = S.union s (S.fromList c)

neighbors :: Point -> [Point]
neighbors (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

solve :: IO (Int, Int)
solve = do
  input <- program <$> getInput "2019" "15"
  let p2mp = p2Map $ initialState input
      o2 = fst . head $ filter (\(_, y) -> y == 2) $ M.assocs p2mp
  return (p1 $ initialState input, bfs o2 p2mp)
