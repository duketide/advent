module Y2019.Day7 (solve) where

import AOC (getInput, readInt)
import Data.List (nub, permutations)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import IntCom (HaltOrAwait (Await, Halt), Program, Return (R, outputs, rb, state, status), execute, loadProg, program)

type AmpState = Map Amp Return

data Amp = A | B | C | D | E deriving (Eq, Ord)

next :: Amp -> Amp
next c = case c of
  A -> B
  B -> C
  C -> D
  D -> E
  E -> A

singleRun :: Program -> [Int] -> Int
singleRun prog = go 0
  where
    go o [] = o
    go o (p : ps) = go nextO nextP
      where
        nextP = ps
        nextO = head . outputs $ execute 0 [p, o] 0 prog

looper :: AmpState -> [Int] -> Int
looper = go A [0]
  where
    go :: Amp -> [Int] -> AmpState -> [Int] -> Int
    go curr inp ampSt p
      | finished = last $ outputs winner
      | otherwise = go (next curr) (reverse $ outputs res) nextState nextPhases
      where
        res = execute ip inputs 0 prog
        inputs = [head p | not (null p)] ++ inp
        (ip, prog) = state $ ampSt M.! curr
        nextPhases = if null p then [] else tail p
        nextState = M.insert curr res ampSt
        winner = if curr == E then res else ampSt M.! E
        finished = all (\x -> status x == Halt) $ M.elems ampSt

p1 :: Program -> Int
p1 p = maximum $ singleRun p <$> permutations [0 .. 4]

p2 :: Program -> Int
p2 p = maximum $ looper (initState p) <$> permutations [5 .. 9]

initState :: Program -> AmpState
initState p = foldr (`M.insert` r) M.empty [A, B, C, D, E]
  where
    r = R {status = Await, state = (0, p), rb = 0, outputs = []}

solve :: IO (Int, Int)
solve = do
  input <- program <$> getInput "2019" "7"
  return (p1 input, p2 input)
