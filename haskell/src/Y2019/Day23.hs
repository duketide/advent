module Y2019.Day23 (solve) where

import AOC (getInput)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import IntCom (Return (R, outputs), execute, program)

solve :: IO (Int, Int)
solve = do
  input <- program <$> getInput "2019" "23"
  let starter = foldr (\x acc -> M.insert x (execute 0 [x] 0 input) acc) M.empty [0 .. 49]
  return (p1 starter, p2 starter)

p1 :: Map Int Return -> Int
p1 mp
  | M.member 255 inps = last $ inps M.! 255
  | otherwise = p1 comps
  where
    (comps, inps) = turn mp

p2 :: Map Int Return -> Int
p2 mp = go mp (-1, -1) False S.empty
  where
    go c nat@(nx, ny) bl check
      | sendNAT && S.member ny check = ny
      | otherwise = go comps nextNat nextBl nextCheck
      where
        (comps', inps) = turn c
        comps = if sendNAT then M.insert 0 (execute zeroIP [nx, ny] zeroRB zeroProg) comps' else comps'
        (R _ (zeroIP, zeroProg) zeroRB _) = comps' M.! 0
        nextBl = not sendNAT && null inps
        sendNAT = bl && null inps
        nextNat = if sendNAT then (-1, -1) else maybe nat ((\(y : x : _) -> (x, y)) . take 2 . reverse) $ M.lookup 255 inps
        nextCheck = if sendNAT then S.singleton ny else check

turn :: Map Int Return -> (Map Int Return, Map Int [Int])
turn mp = (foldr (runOne inps) M.empty . M.assocs $ mp, inps)
  where
    inps = gatherInputs mp

runOne :: Map Int [Int] -> (Int, Return) -> Map Int Return -> Map Int Return
runOne inps (n, ret) comps = nextComps
  where
    (R status (ip, prog) rb _) = ret
    inputs = fromMaybe [-1] $ M.lookup n inps
    nextR = execute ip inputs rb prog
    nextComps = M.insert n nextR comps

gatherInputs :: Map Int Return -> Map Int [Int]
gatherInputs = foldr f M.empty
  where
    f ret acc = nextInps
      where
        rawInps = fmap reverse . chunksOf 3 . outputs $ ret
        nextInps = foldl g acc rawInps
        g mp (z : x : y : _) = M.insertWith (++) z [x, y] mp
        g mp _ = mp
