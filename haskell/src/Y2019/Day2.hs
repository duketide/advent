module Y2019.Day2 (solve) where

import AOC (getInput, readInt)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import IntCom (execute, findRes, loadProg)

solve :: IO (Int, Int)
solve = do
  rawInput <- getInput "2019" "2"
  let input = map readInt $ splitOn "," rawInput
      subs = head input : 12 : 2 : drop 3 input
      vals = loadProg subs
      p1 = fst $ execute [] vals
      p2 = findRes 19690720 vals
  return (p1, p2)
