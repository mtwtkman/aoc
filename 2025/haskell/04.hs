module Main04 where

-- I should make a map which has keys of positions and values of character..
-- Current implementation is terribly inefficient by naive iteration.

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

addUniquely :: (Eq a) => a -> [a] -> [a]
addUniquely e xs
  | e `elem` xs = xs
  | otherwise = e : xs

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

data Item = Item {xPos :: Int, yPos :: Int, itemChar :: Char} deriving (Show, Eq)

aggregateAdjancents :: (Int, Int) -> [String] -> [Item]
aggregateAdjancents xy@(x, y) vs =
  let (xl, yl) = matrixSize vs
   in map
        (\f -> Item x y (findItem (f xy) vs))
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

isAccesable :: [Item] -> Bool
isAccesable items = length (filter ((== '@') . itemChar) items) < 4

solvePart1 :: [String] -> Int
solvePart1 xs = go 0 (0, 0)
 where
  (xl, yl) = matrixSize xs
  go acc (x, y)
    | y == yl = acc
    | x == xl = go acc (0, y + 1)
    | otherwise = go (acc + if isRoleOfPaper (x, y) xs && isAccesable (aggregateAdjancents (x, y) xs) then 1 else 0) (x + 1, y)

matrixSize :: [String] -> (Int, Int)
matrixSize xs = (length $ head xs, length xs)

removeRollOfPaper :: (Int, Int) -> [String] -> [String]
removeRollOfPaper (x, y) orig =
  zipWith
    ( \i row ->
        ( if i == y
            then
              take x row <> "." <> drop (x + 1) row
            else
              row
        )
    )
    [0 .. length orig - 1]
    orig

removeRollOfPapers :: [Item] -> [String] -> [String]
removeRollOfPapers [] vs = vs
removeRollOfPapers ((Item{xPos, yPos}) : rest) vs = removeRollOfPapers rest (removeRollOfPaper (xPos, yPos) vs)

scrape :: [String] -> [Item]
scrape xs = go [] (0, 0)
 where
  (xl, yl) = matrixSize xs

  go acc (x, y)
    | y == yl = acc
    | x == xl = go acc (0, y + 1)
    | otherwise =
        let items = aggregateAdjancents (x, y) xs
            currentItem = Item x y '@'
         in go (if isRoleOfPaper (x, y) xs && isAccesable (aggregateAdjancents (x, y) xs) then addUniquely currentItem acc else acc) (x + 1, y)

solvePart2 :: [String] -> Int
solvePart2  = go 0
 where
  go :: Int -> [String] -> Int
  go acc xs =
    let items = scrape xs
        c = length items
     in if c == 0 then acc else go (acc + c) (removeRollOfPapers items xs)

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

example2 =
  [ ".........."
  , ".........."
  , ".........."
  , "....@@...."
  , "...@@@@..."
  , "...@@@@@.."
  , "...@.@.@@."
  , "...@@.@@@."
  , "...@@@@@.."
  , "....@@@..."
  ]
readInput :: String -> IO [String]
readInput f = lines <$> readFile f

runExamplePart1 = solvePart1 example
runExamplePart2 = solvePart2 example
runPart1 = solvePart1 <$> readInput "04.input"
runPart2 = solvePart2 <$> readInput "04.input"

main :: IO ()
main = do
  -- print runExamplePart2
  runPart2 >>= print
