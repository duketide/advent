module Y2019.Day21 (solve) where

import AOC (getInput)
import Data.Char (chr, ord)
import IntCom (Return (outputs), execute, program)

solve :: IO (Int, Int)
solve = do
  input <- program <$> getInput "2019" "21"
  let inp = ord <$> "NOT C T\nNOT B J\nOR J T\nNOT A J\nOR T J\nAND D J \nWALK\n"
      inp2 = ord <$> "NOT C T\nNOT B J\nOR J T\nNOT A J\nOR T J\nAND D J \nOR J T\nAND E T\nAND J T\nAND H J\nOR T J\nRUN\n"
      p1 = head . outputs $ execute 0 inp 0 input
      p2 = head . outputs $ execute 0 inp2 0 input
  return (p1, p2)
