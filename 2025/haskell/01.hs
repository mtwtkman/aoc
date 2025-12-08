module Main where

import Data.String (IsString (fromString))
import System.IO (readFile)

data Direction = L | R deriving (Show)

instance IsString Direction where
  fromString s = case s of
    "R" -> R
    "L" -> L
    _ -> error "unknown"

parseRow :: String -> (Direction, Int)
parseRow (d : xs) = (fromString [d], read xs :: Int)

rotate :: Int -> Direction -> Int -> Int
rotate p L n = let result = p - (n `mod` 100) in if result < 0 then 100 + result else result
rotate p R n = let result = p + (n `mod` 100) in if result >= 100 then result - 100 else result

countPassedZero :: Int -> Direction -> Int -> Int
countPassedZero p d n =  let (a, b) = n `divMod` 100 in a + if p == 0 then 0 else case d of
                                                      L -> if p - b < 0 then 1 else 0
                                                      R -> if p + b > 100 then 1 else 0

readInput :: String -> IO [(Direction, Int)]
readInput f = readFile f >>= \c -> return $ map parseRow (lines c)

next :: Int -> (Direction, Int) -> Int
next p (d, n) = rotate p d n

run :: [(Direction, Int)] -> ([Int], Int, Int)
run = foldl (\(xs, z, pos) row -> let result = next pos row in (xs <> [result], z + (if result == 0 then 1 else 0), result)) ([], 0, 50)

runPart2 :: [(Direction, Int)] -> ([Int], Int, Int)
runPart2 = foldl (\(xs, z, pos) row@(d, n) -> let result = next pos row in (xs <> [result], z + (if result == 0 then 1 else 0) + countPassedZero pos d n, result)) ([], 0, 50)

example = map parseRow ["L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82"]
runExamplePart1 = let (_, x, _) = run example in x
runExamplePart2 = let a@(_, x, _) = runPart2 example in a

solvePart1 = do
  rows <- readInput "1.input"
  let (a, z, p) = run rows
  print z

solvePart2 = do
  rows <- readInput "1.input"
  let (a, z, p) = runPart2 rows
  print z

main :: IO ()
main = do
  -- print runExamplePart2
  solvePart2
