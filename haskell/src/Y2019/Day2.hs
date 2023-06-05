module Y2019.Day2 (solve) where

import AOC (getInput, readInt)
import qualified Data.IntMap as IM
import Data.List.Split (splitOn)
import IntCom (Return (state), execute, findRes, loadProg, program)

solve :: IO (Int, Int)
solve = do
  input <- IM.insert 2 2 . IM.insert 1 12 . program <$> getInput "2019" "2"
  let p1 = ((IM.!) . snd . state $ execute 0 [] input) 0
      p2 = findRes 19690720 input
  return (p1, p2)
