module Y2019.Day13 (solve) where

import AOC (getInput)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set)
import qualified Data.Set as S
import IntCom (HaltOrAwait (Halt), Program, Return (R, outputs, rb, state, status), execute, program)

type Point = (Int, Int)

type Screen = Map Int (Set Point)

p1 :: Program -> Int
p1 p = length . filter (\(z : _ : _ : _) -> z == 2) . chunksOf 3 . outputs $ execute 0 [] 0 p

drawScreen :: [Int] -> Map Int (Set Point)
drawScreen o = foldr mapper M.empty (chunksOf 3 o)
  where
    mapper (z : y : x : _) acc = next
      where
        next = case (x, y) of
          (-1, 0) -> M.insert 5 (S.singleton (z, 0)) acc
          _ -> M.insert z newSet acc
        newSet = S.insert (x, y) $ fromMaybe S.empty $ M.lookup z acc

gameLoop :: Program -> Int
gameLoop = go 0 0 [] (0, 0)
  where
    go :: Int -> Int -> [Int] -> Point -> Program -> Int
    go ip relBase input (bx, by) p
      | isNothing (M.lookup 4 nextScreen) = score
      | otherwise = go nextIp rb (pure nextInput) (bar + nextInput, barY) nextP
      where
        R {status = status, state = (nextIp, nextP), rb = rb, outputs = outputs'} = execute ip input relBase p
        screen = drawScreen outputs'
        nextScreen = drawScreen $ outputs $ execute nextIp [0] rb nextP
        blocks = length $ fromMaybe S.empty $ M.lookup 2 screen
        score = maybe 0 (fst . head . S.toList) $ M.lookup 5 nextScreen
        (ball, _) = head . S.toList $ nextScreen M.! 4
        (ballX, ballY) = head . S.toList $ screen M.! 4
        (bar, barY) = head . S.toList $ fromMaybe (S.singleton (bx, by)) $ M.lookup 3 screen
        nextInput = if ballY == barY - 1 && ballX == bar then 0 else nextInput'
        nextInput' = case ball `compare` bar of
          GT -> 1
          LT -> -1
          EQ -> 0

solve :: IO (Int, Int)
solve = do
  input <- program <$> getInput "2019" "13"
  let input2 = IM.insert 0 2 input
  return (p1 input, gameLoop input2)
