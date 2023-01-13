{-# LANGUAGE TypeApplications #-}

import Data.Char
import Data.List
import Data.List.Split

parseLines :: String -> [[Int]]
parseLines line = map (\rawRange -> map (\rawInt -> read rawInt :: Int) $ splitOn "-" rawRange) $ splitOn "," line

formatData :: [[Int]] -> (Int, Int, Int, Int)
formatData [[x1, x2], [y1, y2]] = (x1, x2, y1, y2)

isCompleteOverlap :: (Int, Int, Int, Int) -> Bool
isCompleteOverlap (x1, x2, y1, y2) = (x1 - y1) * (x2 - y2) <= 0

isPartialOverlap :: (Int, Int, Int, Int) -> Bool
isPartialOverlap (x1, x2, y1, y2) = (x1 - y2) * (x2 - y1) <= 0

main :: IO ()
main =
  do
    rawInput <- readFile "data.txt"
    let input = lines $ rawInput

    print $ sum $ map fromEnum $ map isCompleteOverlap $ map formatData $ map parseLines input
    print $ sum $ map fromEnum $ map isPartialOverlap $ map formatData $ map parseLines input
