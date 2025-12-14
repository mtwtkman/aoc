module Main06 where

import Data.Maybe (mapMaybe)

data Operator = Mul | Add deriving (Show)

execOp :: Operator -> [Int] -> Int
execOp Mul [x, y] = x * y
execOp Mul (x : y : rest) = execOp Mul (x * y : rest)
execOp Add xs = sum xs

opFromString :: String -> Operator
opFromString "+" = Add
opFromString "*" = Mul
opFromString _ = error "unkonwn"

opFromChar :: Char -> Operator
opFromChar '+' = Add
opFromChar '*' = Mul
onFromChar _ = error "unknown"

splitByBlank :: String -> [String]
splitByBlank = go []
 where
  go :: [String] -> String -> [String]
  go acc [] = acc
  go acc xs =
    let (vs, rem) = span (/= ' ') xs
        rest = dropWhile (== ' ') rem
     in go (acc <> [vs]) rest

parsePart1 :: String -> [(Operator, [Int])]
parsePart1 src =
  let rows = lines src
      ops = map opFromString (splitByBlank $ last rows)
      calcSize = length ops
      paramSize = length rows - 1
      paramRows :: [[Int]] = [map read (splitByBlank (rows !! i)) | i <- [0 .. paramSize - 1]]
   in zip ops ([[paramRows !! j !! i | j <- [0 .. paramSize - 1]] | i <- [0 .. calcSize - 1]])

data Block = Block {blockOp :: Operator, blockSize :: Int} deriving (Show)

parseBlocks :: String -> [Block]
parseBlocks = go []
 where
  go :: [Block] -> String -> [Block]
  go acc "" = let Block{blockOp, blockSize} = last acc in init acc <> [Block blockOp (blockSize + 2)]
  go acc (c : xs) = go (acc <> [Block (opFromChar c) (length (takeWhile (== ' ') xs))]) (dropWhile (== ' ') xs)

divideByBlockSize :: [String] -> [Block] -> [[String]]
divideByBlockSize rows blocks =
  let steps = scanl (\(d, t) b -> (d + t + 1, blockSize b)) (0, blockSize $ head blocks) (tail blocks)
   in map (\r -> map (\(d, t) -> take t (drop d r)) steps) rows

rotate :: [[String]] -> [[String]]
rotate l@(h : _) = go [] 0
 where
  term = length h
  go :: [[String]] -> Int -> [[String]]
  go acc i
    | i == term = acc
    | otherwise = go (map (!! i) l : acc) (i + 1)

buildCalcElems :: [[String]] -> [Block] -> [(Operator, [Int])]
buildCalcElems dividedArgs blocks = zipWith (curry (\(Block{blockOp}, a) -> (blockOp, map read a))) blocks dividedArgs

buildCalcMap :: [String] -> [(Operator, [Int])]
buildCalcMap rows =
  let blocks = parseBlocks (last rows)
      args = rotate $ divideByBlockSize (init rows) blocks
   in zipWith (curry (\(Block{blockOp}, a) -> (blockOp, finalizeNum a))) (reverse blocks) args
 where
  finalizeNum :: [String] -> [Int]
  finalizeNum args@(h:_) = map (read . (\i ->  map (!! i) args)) [0..length h - 1]

solvePart2 :: String -> Int
solvePart2 src = sum $ map (uncurry execOp) (buildCalcMap (lines src))

solvePart1 :: String -> Int
solvePart1 src = sum $ map (uncurry execOp) (parsePart1 src)

example =
  unlines
    [ "123 328  51 64 "
    , " 45 64  387 23 "
    , "  6 98  215 314"
    , "*   +   *   + "
    ]

runExample1 = solvePart1 example
runExample2 = solvePart2 example
runPart1 = solvePart1 <$> readFile "06.input"
runPart2 = solvePart2 <$> readFile "06.input"

main :: IO ()
main = runPart2 >>= print
