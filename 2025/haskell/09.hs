module Main09 where

import Data.List (elemIndex, null)
import qualified Data.Set as S

type Position = (Int, Int)
type Edge = Int
type Area = Int

squreEdges :: (Position, Position) -> (Edge, Edge)
squreEdges ((x1, y1), (x2, y2)) = (abs (x1 - x2) + 1, abs (y1 - y2) + 1)

consumeNaively :: [Position] -> [Area]
consumeNaively (p:ps) = go [] ps ps p
  where
    go :: [Area] -> [Position] ->  [Position] -> Position -> [Area]
    go acc [] _ _ = acc
    go acc (p1:src) [] _ = go acc src src p1
    go acc src (p2:rest) p1 = go (acc <> [calcArea (p1, p2)]) src rest p1

calcArea :: (Position, Position) -> Area
calcArea ((x1, y1), (x2, y2)) = abs (x1 - x2) + 1 * abs (y1 - y2) + 1

splitByWith :: Char -> (String -> a) -> String -> (a, a)
splitByWith d f s = case d `elemIndex` s of
  Just i -> (f $ take i s, f $ drop (i + 1) s)
  Nothing -> error "invalid value"

example =
  [ "7,1"
  , "11,1"
  , "11,7"
  , "9,7"
  , "9,5"
  , "2,5"
  , "2,3"
  , "7,3"
  ]

parse :: [String] -> [Position]
parse = map (splitByWith ',' read)

readInput :: String -> IO [String]
readInput f = lines <$> readFile f

solvePart1 :: [String] -> Int
solvePart1 = maximum . consumeNaively . parse

runExample1 = solvePart1 example
runPart1 = solvePart1 <$> readInput "09.input"

main :: IO ()
main = print runExample1 >> runPart1 >>= print


