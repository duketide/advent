module Y2019.Day15 (solve) where

import AOC (getInput)
import Data.Set (Set)
import qualified Data.Set as S
import IntCom (HaltOrAwait (Await), Program, Return (R, outputs, rb, state, status), execute, program)

type Point = (Int, Int)

data MapState = MapState
  { bot :: Point,
    ret :: Return
  }

type TState = (Set Point, [MapState])

initialState :: Program -> TState
initialState p =
  ( S.empty,
    return
      MapState
        { bot = (0, 0),
          ret = R {status = Await, state = (0, p), rb = 0, outputs = [1]}
        }
  )

nextStates :: MapState -> Set Point -> [MapState] -> TState
nextStates MapState {bot = (x, y), ret = R {status = _, state = (ip, prog), rb = rb, outputs = outputs}} seen lst = (nextSeen, nextPos)
  where
    nextPos = filter f [north, south, west, east] ++ lst
    north = MapState {bot = (x, y - 1), ret = execute ip [1] rb prog}
    south = MapState {bot = (x, y + 1), ret = execute ip [2] rb prog}
    west = MapState {bot = (x - 1, y), ret = execute ip [3] rb prog}
    east = MapState {bot = (x + 1, y), ret = execute ip [4] rb prog}
    f MapState {bot = seenCheck, ret = R {status = _, state = _, rb = _, outputs = (x : _)}} = x /= 0 && not (S.member seenCheck seen)
    nextSeen = foldr S.insert seen [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

p1 :: TState -> Int
p1 = go 0
  where
    go n st
      | f st = n
      | otherwise = go (n + 1) $ foldr g (fst st, []) (snd st)
      where
        f = any ((== 2) . head . outputs . ret) . snd
        g x (seen, lst) = nextStates x seen lst

solve :: IO (Int, Int)
solve = do
  input <- program <$> getInput "2019" "15"
  return (p1 $ initialState input, 0)
