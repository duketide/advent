{-# LANGUAGE OverloadedStrings #-}

module AOC (readInt, getInput, trim, nbrs, nbrsBounded, predWrap, succWrap) where

import Control.Lens ((&), (.~), (^.))
import Data.Bits (Bits (xor))
import Data.ByteString (ByteString)
import Data.Char (isSpace, readLitChar)
import Data.List.Split (splitOn)
import Network.Wreq (defaults, getWith, header, responseBody)
import System.Directory (doesFileExist, getHomeDirectory)
import System.Posix.Env.ByteString (getEnv)

type Pair = (Int, Int)

readInt :: String -> Int
readInt = read

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

parseString :: String -> String
parseString s = go s []
  where
    go :: String -> String -> String
    go [] r = reverse r
    go o r = go p (next : r)
      where
        (next, p) = head $ readLitChar o

getInput :: String -> String -> IO String
getInput year day = do
  home <- getHomeDirectory
  let file = home ++ "/coding/advent/input/" ++ year ++ "/day" ++ day ++ ".txt"
  haveInput <- doesFileExist file
  if haveInput
    then readFile file
    else do
      -- need to store "AOC_SESSION" as "session=TOKEN"
      token <- getEnv "AOC_SESSION"
      case token of
        Nothing -> return "busted env variable"
        Just x -> fetch x year day file

fetch :: ByteString -> String -> String -> String -> IO String
fetch token year day file = do
  home <- getHomeDirectory
  let opts = defaults & header "Cookie" .~ [token]
  response <- getWith opts ("https://adventofcode.com/" ++ year ++ "/day/" ++ day ++ "/input")
  let str = parseString $ tail . init . show $ response ^. responseBody
  writeFile file str
  readFile file

nbrs :: Pair -> [Pair]
nbrs (a, b) = [(f a, g b) | f <- funcs, g <- funcs, (f a, g b) /= (a, b)]
  where
    funcs = [(+ 1), (+ (-1)), id]

nbrsBounded :: Pair -> Pair -> Pair -> [Pair]
nbrsBounded (xMin, xMax) (yMin, yMax) (a, b) = [(f a, g b) | f <- funcs, g <- funcs, let x = f a; y = g b, (x, y) /= (a, b), x >= xMin, x <= xMax, y >= yMin, y <= yMax]
  where
    funcs = [(+ 1), (+ (-1)), id]

succWrap :: (Bounded a, Enum a, Eq a) => a -> a
succWrap x
  | x == maxBound = minBound
  | otherwise = succ x

predWrap :: (Bounded a, Enum a, Eq a) => a -> a
predWrap x
  | x == minBound = maxBound
  | otherwise = pred x
