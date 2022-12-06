module Main (main) where

import Data.List (nub)

findMarker :: Int -> String -> Int
findMarker _ [] = error "Reached end of the list without finding marker"
findMarker markLen inpt@(x : xs) = if isUnique then markLen else 1 + findMarker markLen xs
  where
    isUnique = length (nub $ take markLen inpt) == length (take markLen inpt)

findPacketMarker = findMarker 4
findMessageMarker = findMarker 14

main :: IO ()
main = readFile "input.txt" >>= print . findMessageMarker
