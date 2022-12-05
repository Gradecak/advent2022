module Main (main) where

import Control.Monad (void)
import Data.Bifunctor (bimap)

-- Rock, Paper, Scissors
data GameMove = A | B | C deriving (Read, Show)
-- Lose, Draw, Win
data DesiredOutcome = X | Y | Z deriving (Read, Show)

gameScore :: (GameMove, DesiredOutcome) -> Int
gameScore (A, X) = 0 + inputScore C
gameScore (A, Y) = 3 + inputScore A
gameScore (A, Z) = 6 + inputScore B
gameScore (B, X) = 0 + inputScore A
gameScore (B, Y) = 3 + inputScore B
gameScore (B, Z) = 6 + inputScore C
gameScore (C, X) = 0 + inputScore B
gameScore (C, Y) = 3 + inputScore C
gameScore (C, Z) = 6 + inputScore A

inputScore :: GameMove -> Int
inputScore A = 1
inputScore B = 2
inputScore C = 3

parseInput :: String -> (GameMove, DesiredOutcome)
parseInput inp = bimap read read $ splitAt 1 inp

main :: IO ()
main = do
  file <- readFile "./input.txt"
  print $ sum $ map (gameScore . parseInput) $ lines file
