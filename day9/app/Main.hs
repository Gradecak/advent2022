module Main (main) where

import Data.List (nub, sortBy)
import Data.Functor ((<&>))

data Move = L | R  | U  | D
  deriving (Read, Show)

type Position = (Int, Int)


inRange :: Position -> Position -> Bool
inRange (x1, y1) (x2, y2) = xDelta < 2 && yDelta < 2
  where xDelta = abs(x2 - x1)
        yDelta= abs(y2 - y1)

parseMove :: String -> [Move]
parseMove inpt = case words inpt of
  [direction, amnt] -> replicate (read amnt) (read direction)
  _ -> error "Failed to parse move: "

distance :: Position -> Position -> Float
distance (x1 , y1) (x2 , y2) = sqrt $ fromIntegral (x'*x' + y'*y')
    where x' = x1 - x2
          y' = y1 - y2

sumPositions :: Position -> Position -> Position
sumPositions (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

makeMove :: Move -> Position -> Position
makeMove L (x, y) = (x-1, y)
makeMove R (x, y) = (x+1, y)
makeMove U (x, y) = (x, y+1)
makeMove D (x, y) = (x, y-1)

moveHead :: [Position] -> Move -> [Position]
moveHead [] move = moveHead [(0,0)] move
moveHead positions move = makeMove move (head positions) : positions

moveTail :: Position -> [Position]  -> [Position]
moveTail hPos [] = moveTail hPos [(0,0)]
moveTail hPos pos@(tPos:_)= if inRange tPos hPos
  then tPos:pos
  else head possibilities : pos
  where
    distanceToHead = distance hPos
    possibilities = sortBy (\a b -> compare (distanceToHead a) (distanceToHead b)) $ map (sumPositions tPos) ((,) <$> [-1..1] <*> [-1..1])

headPositions ::  [Move] -> [Position]
headPositions = foldl moveHead []

tailPositions :: [Position] -> [Position]
tailPositions = foldr moveTail []


part1 :: IO ()
part1 = do
  moves <- readFile "./input.txt" <&> concatMap parseMove . lines
  print $ length $ nub $ tailPositions (headPositions moves)


segmentPoints :: Int -> [Position] -> [[Position]]
segmentPoints 0 _ = []
segmentPoints n inp = newHead : segmentPoints (n-1) newHead
  where newHead = tailPositions inp


part2 :: IO ()
part2 = do
  moves <- readFile "./input.txt" <&> concatMap parseMove . lines
  let headMoves = headPositions moves
      segmentMovements = segmentPoints 9 headMoves
  print $ length $ nub $ last segmentMovements


main :: IO ()
main = part1 >> part2
