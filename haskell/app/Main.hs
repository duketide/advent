module Main where

import AOC (readInt)
import Y2019.Day1
import Y2019.Day10
import Y2019.Day11
import Y2019.Day12
import Y2019.Day13
import Y2019.Day14
import Y2019.Day15
import Y2019.Day16
import Y2019.Day17
import Y2019.Day18
import Y2019.Day19
import Y2019.Day2
import Y2019.Day3
import Y2019.Day4
import Y2019.Day5
import Y2019.Day6
import Y2019.Day7
import Y2019.Day8
import Y2019.Day9

report :: Show a => Show b => Int -> IO (a, b) -> IO ()
report d x = do
  (a, b) <- x
  putStrLn $ "Day " ++ show d ++ " Part 1: " ++ show a ++ ", Part 2: " ++ show b

solutions =
  [ report 1 Y2019.Day1.solve,
    report 2 Y2019.Day2.solve,
    report 3 Y2019.Day3.solve,
    report 4 Y2019.Day4.solve,
    report 5 Y2019.Day5.solve,
    report 6 Y2019.Day6.solve,
    report 7 Y2019.Day7.solve,
    report 8 Y2019.Day8.solve,
    report 9 Y2019.Day9.solve,
    report 10 Y2019.Day10.solve,
    report 11 Y2019.Day11.solve,
    report 12 Y2019.Day12.solve,
    report 13 Y2019.Day13.solve,
    report 14 Y2019.Day14.solve,
    report 15 Y2019.Day15.solve,
    report 16 Y2019.Day16.solve,
    report 17 Y2019.Day17.solve,
    report 18 Y2019.Day18.solve,
    report 19 Y2019.Day19.solve
  ]

f :: String -> IO [()]
f i
  | i `elem` (show <$> [1 .. length solutions]) = sequence [solutions !! (readInt i - 1)]
  | i == "all" = sequence solutions
  | otherwise = sequence [last solutions]

main :: IO [()]
main = do
  putStrLn "Which day? (default is latest, enter # or \"all\" for a specific day or all days, repsectively)"
  input <- getLine
  f input
