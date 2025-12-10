module Main03 where

import Data.Functor ((<&>))
import System.IO (readFile)

parse :: String -> [Int]
parse = map (read . (: []))

readInput :: String -> IO [String]
readInput f = lines <$> readFile f

searchMaxWithIndex :: [Int] -> (Int, Int)
searchMaxWithIndex xs = go (head xs) 0 1 (tail xs)
 where
  go :: Int -> Int -> Int -> [Int] -> (Int, Int)
  go x i _ [] = (x, i)
  go x i c (n : ns)
    | x >= n = go x i (c + 1) ns
    | otherwise = go n (i + c) 1 ns

solvePart1 :: [String] -> Int
solvePart1 rows = sum [detectJolt xs (search1stBatteryWithIndex xs) | xs <- map parse rows]
 where
  search1stBatteryWithIndex :: [Int] -> (Int, Int)
  search1stBatteryWithIndex = searchMaxWithIndex . init

  detectJolt :: [Int] -> (Int, Int) -> Int
  detectJolt xs (n, i) = 10 * n + fst (searchMaxWithIndex (drop (i + 1) xs))

example =
  unlines
    [ "987654321111111"
    , "811111111111119"
    , "234234234234278"
    , "818181911112111"
    ]

solvePart2 :: Int -> [String] -> Int
solvePart2 d rows = sum [sum $ go d [] row | row <- map parse rows]
 where
  go :: Int -> [Int] -> [Int] -> [Int]
  go 1 b xs = b <> [fst $ searchMaxWithIndex xs]
  go d' b xs
    | d' == length xs  = b <> zipWith (\ x e -> x * (10 ^ e)) xs (reverse [0..(d' - 1)])
    | otherwise =
        let (n, i) = searchMaxWithIndex (take (length xs - (d' - 1)) xs)
         in go (d' - 1) (n * (10 ^ (d' - 1)) : b) (drop (i + 1) xs)

runExamplePart1 = solvePart1 $ lines example
runPart1 = solvePart1 <$> readInput "03.input"
runExamplePart2 = solvePart2 12 $ lines example
runPart2 = solvePart2 12 <$> readInput "03.input"

main :: IO ()
main = runPart2 >>= print
