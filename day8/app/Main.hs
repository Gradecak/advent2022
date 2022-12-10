module Main (main) where

import Data.List (singleton, transpose)

readNumberList :: [Char] -> [Int]
readNumberList = map (read . singleton)

rowVisibility' :: Int -> [Int] -> [Bool]
rowVisibility' _ [] = []
rowVisibility' currMax (x:xs) = (currMax < x)  : (rowVisibility' (max currMax x) xs)

rowVisibility :: [Int] -> [Bool]
rowVisibility row = zipWith (||) left right
  where left = rowVisibility' (-1) row
        right = reverse $ rowVisibility' (-1) $ reverse row

part1 :: IO ()
part1 = do
  input <- readFile "./input.txt" >>= return . map readNumberList . lines
  let eastWest = map rowVisibility input
      northSouth = transpose (map rowVisibility (transpose input))
      visibilityMap = zipWith (zipWith (||)) eastWest northSouth
  print $ sum $ map (sum . map fromEnum) visibilityMap


scoreForRow :: [Int] -> Int-> Int
scoreForRow row idx = scoreLeft * scoreRight
  where (left, (elt:right)) = splitAt idx row
        (leftView, leftRemainder) = span (< elt) (reverse left)
        (rightView, rightRemainder) = span (< elt) right
        scoreLeft = length leftView+ (fromEnum . not . null) leftRemainder
        scoreRight = length rightView + (fromEnum . not . null) rightRemainder

scoreForMatrix :: [[Int]]  -> [[Int]]
scoreForMatrix rows = map (\row -> map (scoreForRow row) (take (length rows) [0..])) rows

part2 :: IO ()
part2 = do
  input <- readFile "./input.txt" >>= return . map readNumberList . lines
  let northSouth = input
      eastWest = transpose northSouth
      scoreNorthSouth = scoreForMatrix northSouth
      scoreEastWest = transpose $ scoreForMatrix eastWest
      scoreMatrix = zipWith (zipWith (*)) scoreNorthSouth scoreEastWest
  print $ maximum (map maximum scoreMatrix)
  return ()

main:: IO ()
main = part1 >> part2
