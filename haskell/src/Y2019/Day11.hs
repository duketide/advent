module Y2019.Day11 (solve) where

import AOC (getInput, predWrap, succWrap)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import IntCom (HaltOrAwait (Halt), Program, Return (R, outputs, state, status), execute, program)

type Point = (Int, Int)

data Facing = N | E | S | W deriving (Eq, Ord, Show, Bounded, Enum)

data ListDetails = LD
  { xMin :: Int,
    xMax :: Int,
    yMin :: Int,
    yMax :: Int
  }
  deriving (Show)

travel :: Facing -> Point -> Point
travel face (x, y) = case face of
  N -> (x, y - 1)
  E -> (x + 1, y)
  S -> (x, y + 1)
  W -> (x - 1, y)

data Robot = Robot
  { facing :: Facing,
    pos :: Point
  }

looper :: Program -> Map Point Int -> Map Point Int
looper p = go p 0 (Robot {facing = N, pos = (1, 1)})
  where
    go :: Program -> Int -> Robot -> Map Point Int -> Map Point Int
    go p ip Robot {facing = facing, pos = pos} mp
      | status == Halt = mp
      | otherwise = go nextP nextIp nextR nextMap
      where
        R {outputs = (dir : color : _), state = (nextIp, nextP), status = status} = execute ip (pure inp) p
        inp = fromMaybe 0 $ M.lookup pos mp
        nextMap = M.insert pos color mp
        nextFacing = let f = if dir == 0 then left else right in f facing
        nextR = Robot {facing = nextFacing, pos = travel nextFacing pos}
        left = predWrap
        right = succWrap

draw :: Map Point Int -> String
draw mp = go xmx ymx []
  where
    LD {xMin = xmn, xMax = xmx, yMin = ymn, yMax = ymx} = getMaxesMins $ getOnes $ M.assocs mp
    go x y m
      | x == xmn && y == ymn = unlines $ chunksOf (xmx - xmn + 1) n
      | x == xmn = go xmx (y - 1) n
      | otherwise = go (x - 1) y n
      where
        n = g (fromMaybe 0 $ M.lookup (x, y) mp) : m
        g 1 = '#'
        g 0 = ' '
        g _ = '!'

getOnes :: [(Point, Int)] -> [Point]
getOnes l = [fst x | x <- l, snd x == 1]

getMaxesMins :: [Point] -> ListDetails
getMaxesMins l = LD {xMin = minimum xs, xMax = maximum xs, yMin = minimum ys, yMax = maximum ys}
  where
    xs = fst <$> l
    ys = snd <$> l

p1Solve :: Program -> Int
p1Solve = length . flip looper M.empty

p2Solve :: Program -> String
p2Solve = draw . flip looper (M.singleton (1, 1) 1)

solve :: IO (Int, String)
solve = do
  input <- program <$> getInput "2019" "11"
  putStrLn $ p2Solve input
  return (p1Solve input, "see print")
