module IntCom
  ( execute,
    program,
    findRes,
    loadProg,
    Return (R),
    status,
    state,
    outputs,
    rb,
    HaltOrAwait (Halt, Await),
    Program,
  )
where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

type Program = IntMap Int

data HaltOrAwait = Halt | Await deriving (Eq, Ord, Show)

data Return = R
  { status :: HaltOrAwait,
    state :: (Int, Program), -- the Int is the instruction pointer
    rb :: Int, -- relative base
    outputs :: [Int]
  }
  deriving (Show)

padLength = 5

pad :: String -> String
pad s
  | length s == padLength = s
  | otherwise = pad ('0' : s)

(#&) :: IntMap Int -> Int -> Int
(#&) m k = fromMaybe 0 $ IM.lookup k m

execute :: Int -> [Int] -> Int -> Program -> Return
execute start inp = go inp [] start
  where
    go :: [Int] -> [Int] -> Int -> Int -> Program -> Return
    go i o pos r m = case opCode of
      99 -> R {status = Halt, state = (pos, m), rb = r, outputs = o}
      1 -> go i o (pos + 4) r $ IM.insert three sm m
      2 -> go i o (pos + 4) r $ IM.insert three pd m
      3 -> if null i then R {status = Await, state = (pos, m), rb = r, outputs = o} else go (tail i) o (pos + 2) r $ IM.insert one (head i) m
      4 -> go i (m #& one : o) (pos + 2) r m
      5 -> go i o (if isTrue then m #& two else pos + 3) r m
      6 -> go i o (if isFalse then m #& two else pos + 3) r m
      7 -> go i o (pos + 4) r lessThan
      8 -> go i o (pos + 4) r equals
      9 -> go i o (pos + 2) (r + m #& one) m
      _ -> error ("bad opCode of " ++ show opCode ++ " at pos " ++ show pos ++ " which is " ++ show (m IM.! pos))
      where
        inst = pad $ show $ m #& pos
        opCode = read $ drop (padLength - 2) inst
        one = f pOne (pos + 1)
        two = f pTwo (pos + 2)
        three = f pThree (pos + 3)
        pOne = inst !! max 0 (padLength - 3)
        pTwo = inst !! max 0 (padLength - 4)
        pThree = inst !! max 0 (padLength - 5)
        sm = m #& one + m #& two
        pd = m #& one * m #& two
        isTrue = m #& one /= 0
        isFalse = m #& one == 0
        lessThan = IM.insert three (if m #& one < m #& two then 1 else 0) m
        equals = IM.insert three (if m #& one == m #& two then 1 else 0) m
        f p
          | p == '1' = id
          | p == '0' = (#&) m
          | otherwise = (+ r) . (#&) m

findRes :: Int -> Program -> Int
findRes res = go 99 99
  where
    go noun verb mp
      | answer == res = 100 * noun + verb
      | verb == -1 = go (noun - 1) 99 mp
      | otherwise = go noun (verb - 1) mp
      where
        ansMap = snd . state $ execute 0 [] 0 $ IM.insert 2 verb (IM.insert 1 noun mp)
        answer = ansMap IM.! 0

program :: String -> Program
program l = loadProg $ read <$> splitOn "," l

loadProg :: [Int] -> Program
loadProg = fst . foldl (\(m, n) x -> (IM.insert n x m, n + 1)) (IM.empty, 0)
