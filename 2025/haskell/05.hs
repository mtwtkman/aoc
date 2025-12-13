module Main05 where

import Data.List (nub)

splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy f xs = (takeWhile f xs, tail $ dropWhile f xs)

parseRange :: String -> (Int, Int)
parseRange xs = let (s, e) = splitBy (/= '-') xs in (read s :: Int, read e :: Int)

parse :: String -> ([(Int, Int)], [Int])
parse src =
  let rows = lines src
      (ranges, ids) = splitBy (/= "") rows
   in (map parseRange ranges, map read ids)

between :: (Eq a, Ord a) => a -> (a, a) -> Bool
between x (s, e) = s <= x && x <= e

solvePart1 :: String -> Int
solvePart1 = length . nub . go [] . parse
 where
  go :: [Int] -> ([(Int, Int)], [Int]) -> [Int]
  go acc (_, []) = acc
  go acc (ranges, id : ids) = if any (between id) ranges then go (id : acc) (ranges, ids) else go acc (ranges, ids)

isSeparated :: (Int, Int) -> (Int, Int) -> Bool
isSeparated (s1, e1) (s2, e2) = (max s1 s2 - min e1 e2) > 1

mergeRanges :: [(Int, Int)] -> (Int, Int)
mergeRanges rs = (minimum (map fst rs), maximum (map snd rs))

alignRanges :: [(Int, Int)] -> [(Int, Int)]
alignRanges = go []
  where
    go :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
    go acc [] = acc
    go acc (x:xs) = let merged = mergeRanges (x : filter (not . isSeparated x) (acc <> xs))
                        separated = filter (isSeparated x) (acc <> xs)
                     in go (merged : separated) xs

solvePart2 :: String -> Int
solvePart2 xs = sum $ map (\(s, e) -> e - s + 1) (alignRanges (fst $ parse xs))

example =
  unlines
    [ "3-5"
    , "10-14"
    , "16-20"
    , "12-18"
    , ""
    , "1"
    , "5"
    , "8"
    , "11"
    , "17"
    , "32"
    ]

runExample1 = solvePart1 example
runExample2 = solvePart2 example
runPart1 = readFile "05.input" >>= print . solvePart1
runPart2 = readFile "05.input" >>= print . solvePart2

main :: IO ()
main = runPart2
