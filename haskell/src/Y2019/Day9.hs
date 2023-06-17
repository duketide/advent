module Y2019.Day9 (solve) where

import AOC (getInput)
import IntCom (Return (outputs), execute, program)

solve :: IO (Int, Int)
solve = do
  input <- program <$> getInput "2019" "9"
  let p1 = head $ outputs $ execute 0 [1] 0 input
  let p2 = head $ outputs $ execute 0 [2] 0 input
  return (p1, p2)
