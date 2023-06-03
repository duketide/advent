module Main where

import Y2019.Day1
import Y2019.Day2
import Y2019.Day3
import Y2019.Day4
import Y2019.Day5
import Y2019.Day6

report :: Show a => Show b => Int -> IO (a, b) -> IO ()
report d x = do
  (a, b) <- x
  putStrLn $ "Day " ++ show d ++ " Part 1: " ++ show a ++ ", Part 2: " ++ show b

main :: IO ()
main = do
  report 1 Y2019.Day1.solve
  report 2 Y2019.Day2.solve
  report 3 Y2019.Day3.solve
  report 4 Y2019.Day4.solve
  report 5 Y2019.Day5.solve
  report 6 Y2019.Day6.solve
