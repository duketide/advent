module Y2019.Day5 (solve) where

import AOC (getInput, readInt)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import IntCom (execute, loadProg)

solve :: IO (Int, Int)
solve = do
  rawInput <- getInput "2019" "5"
  let input = loadProg $ map readInt $ splitOn "," rawInput
      p1 = head . snd $ execute [1] input
      p2 = head . snd $ execute [5] input
  return (p1, p2)
