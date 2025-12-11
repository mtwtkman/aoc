module Main04 where

findItem :: (Int, Int) -> [String] -> Char
findItem (x, y) rows = rows !! y !! x

data Pos = UpperLeft | Upper | UpperRight | RightSide | BottomRight | Bottom | BottomLeft | LeftSide | Inside deriving (Show)

pos :: (Int, Int) -> Int -> Int -> Pos
pos (0, 0) _ _ = UpperLeft
pos (x, y) l1 l2
  | x == l1 && y == l2 = BottomRight
  | x == l1 && y == 0 = UpperRight
  | x == 0 && y == l2 = BottomLeft
  | x == 0 = LeftSide
  | y == 0 = Upper
  | x == l1 = RightSide
  | y == l2 = Bottom
  | otherwise = Inside

moveUpper :: (Int, Int) -> (Int, Int)
moveUpper (x, y) = (x, y - 1)

moveBottom :: (Int, Int) -> (Int, Int)
moveBottom (x, y) = (x, y + 1)

moveRight :: (Int, Int) -> (Int, Int)
moveRight (x, y) = (x + 1, y)

moveLeft :: (Int, Int) -> (Int, Int)
moveLeft (x, y) = (x - 1, y)

moveUpperLeft = moveUpper . moveLeft
moveBottomLeft = moveBottom . moveLeft
moveUpperRight = moveUpper . moveRight
moveBottomRight = moveBottom . moveRight

aggregateAdjancents :: (Int, Int) -> [String] -> [Char]
aggregateAdjancents xy vs =
  let (xl, yl) = matrixSize vs
   in map
        (\f -> findItem (f xy) vs)
        ( case pos xy (xl - 1) (yl - 1) of
            UpperLeft -> [moveRight, moveBottom, moveBottomRight]
            Upper -> [moveLeft, moveRight, moveBottomRight, moveBottom, moveBottomLeft]
            UpperRight -> [moveLeft, moveBottom, moveBottomLeft]
            RightSide -> [moveUpperLeft, moveUpper, moveBottom, moveBottomLeft, moveLeft]
            BottomRight -> [moveUpperLeft, moveUpper, moveLeft]
            Bottom -> [moveUpperLeft, moveUpper, moveUpperRight, moveRight, moveLeft]
            BottomLeft -> [moveUpper, moveUpperRight, moveRight]
            LeftSide -> [moveUpper, moveUpperRight, moveRight, moveBottomRight, moveBottom]
            Inside -> [moveUpperLeft, moveUpper, moveUpperRight, moveRight, moveBottomRight, moveBottom, moveBottomLeft, moveLeft]
        )

isRoleOfPaper :: (Int, Int) -> [String] -> Bool
isRoleOfPaper pos xs = findItem pos xs == '@'

countRoleOfPaper :: (Int, Int) -> [String] -> Int
countRoleOfPaper pos vs = length $ filter (== '@') (aggregateAdjancents pos vs)

solvePart1 :: [String] -> Int
solvePart1 xs = go 0 (0, 0)
  where
    (xl, yl) = matrixSize xs
    go acc (x, y)
      | y == yl = acc
      | x == xl = go acc (0, y + 1)
      | otherwise = go (acc + if countRoleOfPaper (x, y) xs < 4 && isRoleOfPaper (x, y) xs then 1 else 0) (x + 1, y)

matrixSize :: [String] -> (Int, Int)
matrixSize xs = (length $ head xs, length xs)

example =
    [ "..@@.@@@@."
    , "@@@.@.@.@@"
    , "@@@@@.@.@@"
    , "@.@@@@..@."
    , "@@.@@@@.@@"
    , ".@@@@@@@.@"
    , ".@.@.@.@@@"
    , "@.@@@.@@@@"
    , ".@@@@@@@@."
    , "@.@.@@@.@."
    ]

readInput :: String -> IO [String]
readInput f = lines <$> readFile f

runExamplePart1 = solvePart1 example
runPart1 = solvePart1 <$> readInput "04.input"

main :: IO ()
main =  runPart1 >>= print
