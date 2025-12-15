module Main07 where

import Data.Foldable (asum)
import Data.List (elemIndex, elemIndices, nub)
import Data.Maybe (fromMaybe)

getStartPosition :: String -> Int
getStartPosition s = fromMaybe (error "invalid") (elemIndex 'S' s)

getSplitterPositions :: String -> [Int]
getSplitterPositions = elemIndices '^'

addUniquely :: (Eq a) => a -> [a] -> [a]
addUniquely e xs
  | e `elem` xs = xs
  | otherwise = e : xs

concatUniquely :: (Eq a) => [a] -> [a] -> [a]
concatUniquely a b = foldl (flip addUniquely) b a

splittedInfo :: [Int] -> String -> (Int, [Int])
splittedInfo beams row =
  let hits = nub $ filter (`elem` getSplitterPositions row) beams
      thorugh = filter (\b -> b `notElem` getSplitterPositions row) beams
   in (length hits, foldl (\acc i -> concatUniquely [i - 1, i + 1] acc) [] hits <> thorugh)

removePathRows :: [String] -> [String]
removePathRows = filter (any (/= '.'))

data Splitter = Splitter {splitterPosition :: Int, splitCount :: Int} deriving (Show)

beamLine :: Splitter -> (Int, Int)
beamLine (Splitter{splitterPosition}) = (splitterPosition - 1, splitterPosition + 1)

splitter :: [String] -> [(Int, [Splitter])]
splitter rows =
  foldr
    ( \(i, row) acc ->
        ( i
        , foldl
            ( \acc' (j, c) ->
                if c == '^' then acc' <> [Splitter j (countBeam acc j)] else acc'
            )
            []
            (zip [0 ..] row)
        )
          : acc
    )
    []
    (zip [0 ..] rows)

detectSplittedCount :: [Splitter] -> Int -> Int
detectSplittedCount [] _ = 0
detectSplittedCount s@(Splitter{splitterPosition, splitCount} : xs) pos
  | splitterPosition == pos = splitCount
  | otherwise = detectSplittedCount xs pos

calcSplittedCount :: [(Int, [Splitter])] -> Int -> Int
calcSplittedCount src pos = go 1 src
 where
  go :: Int -> [(Int, [Splitter])] -> Int
  go acc [] = acc
  go acc ((_, x) : rest) = case detectSplittedCount x pos of
    0 -> go acc rest
    c -> c

countBeam :: [(Int, [Splitter])] -> Int -> Int
countBeam sp pos = sum $ map (calcSplittedCount sp) [pos - 1, pos + 1]

solvePart1 :: [String] -> Int
solvePart1 src =
  let rows = removePathRows src
      start = getStartPosition (head rows)
   in snd $ foldl (\(ps, cnt) row -> let (hits, nextPositions) = splittedInfo ps row in (nextPositions, cnt + hits)) ([start], 0) (tail rows)

solvePart2 :: [String] -> Int
solvePart2 src = splitCount $ head $ snd $ head $ dropWhile (null . snd) (splitter src)

example =
  [ ".......S......."
  , "..............."
  , ".......^......."
  , "..............."
  , "......^.^......"
  , "..............."
  , ".....^.^.^....."
  , "..............."
  , "....^.^...^...."
  , "..............."
  , "...^.^...^.^..."
  , "..............."
  , "..^...^.....^.."
  , "..............."
  , ".^.^.^.^.^...^."
  , "..............."
  ]

readInput :: String -> IO [String]
readInput f = lines <$> readFile f

runExample1 = solvePart1 example
runExample2 = solvePart2 example
runPart1 = solvePart1 <$> readInput "07.input"
runPart2 = solvePart2 <$> readInput "07.input"

main :: IO ()
main = do
  print runExample2
  runPart2 >>= print
