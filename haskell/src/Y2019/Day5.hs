module Y2019.Day5 (solve) where

import AOC (getInput, readInt)
import Data.List.Split (splitOn)
import IntCom (Return (outputs), execute, loadProg, program)

solve :: IO (Int, Int)
solve = do
  input <- program <$> getInput "2019" "5"
  let p1 = head . outputs $ execute 0 [1] input
      p2 = head . outputs $ execute 0 [5] input
  return (p1, p2)
