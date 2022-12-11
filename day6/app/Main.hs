module Main (main) where

import Data.Functor ((<&>))
import Data.List (nub)

findMarker :: Int -> String -> Maybe Int
findMarker _ [] = Nothing
findMarker markLen inpt@(x : xs)
  | length (nub $ take markLen inpt) == length (take markLen inpt) = Just markLen
  | otherwise = findMarker markLen xs <&> (+ 1)

findPacketMarker = findMarker 4

findMessageMarker = findMarker 14

main :: IO ()
main = readFile "input.txt" >>= print . findMessageMarker
