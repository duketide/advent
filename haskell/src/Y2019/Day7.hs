module Y2019.Day7 (solve) where

import AOC (getInput, readInt)
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import IntCom (HaltOrAwait (Await, Halt), Return (R, outputs, state, status), execute, loadProg)

type AmpState = Map Amp Return

type Program = Map Int Int

data Amp = A | B | C | D | E deriving (Eq, Ord)

next :: Amp -> Amp
next c = case c of
  A -> B
  B -> C
  C -> D
  D -> E
  E -> A

singleRun :: Map Int Int -> [Int] -> Int
singleRun prog = go 0
  where
    go o [] = o
    go o p = go nextO nextP
      where
        nextP = tail p
        nextO = head . outputs $ execute 0 [head p, o] prog

looper :: AmpState -> [Int] -> Int
looper = go A [0]
  where
    go :: Amp -> [Int] -> AmpState -> [Int] -> Int
    go curr inp ampSt p
      | finished = last $ outputs winner
      | otherwise = go (next curr) (reverse $ outputs res) nextState nextPhases
      where
        res = execute ip inputs prog
        inputs = [head p | not (null p)] ++ inp
        (ip, prog) = state $ ampSt M.! curr
        nextPhases = if null p then [] else tail p
        nextState = M.insert curr res ampSt
        winner = if curr == E then res else ampSt M.! E
        finished = all (\x -> status x == Halt) $ M.elems ampSt

combos :: [Int] -> [[Int]]
combos [] = [[]]
combos xs = [x : y | x <- xs, y <- combos $ filter (/= x) xs]

solve :: IO (Int, Int)
solve = do
  rawInput <- getInput "2019" "7"
  let input = loadProg $ map readInt $ splitOn "," rawInput
      p1c = combos [0 .. 4]
      p1 = maximum $ map (singleRun input) p1c
      p2c = combos [5 .. 9]
      r = R {status = Await, state = (0, input), outputs = []}
      p2state = foldr (`M.insert` r) M.empty [A, B, C, D, E]
      p2 = maximum $ map (looper p2state) p2c
  return (p1, p2)
