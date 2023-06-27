module Y2019.Day25 (solve) where

import AOC (getInput)
import Data.Char (chr, ord)
import IntCom (HaltOrAwait (Await, Halt), Program, Return (R), execute, program)

solve :: IO (String, String)
solve = do
  input <- program <$> getInput "2019" "25"
  let ret = execute 0 [0] 0 input
  p1 <- play ret
  return (p1, "none")

-- take hypercube, shell, candy cane, weather machine

play :: Return -> IO String
play (R status (ip, prog) rb op) = case status of
  Halt -> do
    putStrLn $ chr <$> reverse op
    putStrLn "Give answer:"
    getLine
  Await -> do
    putStrLn $ chr <$> reverse op
    inp <- getLine
    play $ execute ip ((ord <$> inp) ++ [10]) rb prog
