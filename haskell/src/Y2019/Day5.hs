module Y2019.Day5 (solve) where

import AOC (getInput, readInt)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import IntCom (Return (outputs), execute, loadProg)

solve :: IO (Int, Int)
solve = do
  rawInput <- getInput "2019" "5"
  let input = loadProg $ map readInt $ splitOn "," rawInput
      p1 = head . outputs $ execute 0 [1] input
      p2 = head . outputs $ execute 0 [5] input
  return (p1, p2)
