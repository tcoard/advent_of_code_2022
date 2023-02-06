{-# LANGUAGE TypeApplications #-}

import Data.Char
import Data.List
import Data.List.Split

seperateCommands :: [[String]] -> ([String], [String])
seperateCommands [rawBoxes, commands] = (init rawBoxes, commands)

-- parseLines line =  splitOn "\n\n" line
-- parseLines line = map (\rawRange -> map (\rawInt -> read rawInt :: Int) $ splitOn "-" rawRange) $ splitOn "," line

formatData :: [[Int]] -> (Int, Int, Int, Int)
formatData [[x1, x2], [y1, y2]] = (x1, x2, y1, y2)

isCompleteOverlap :: (Int, Int, Int, Int) -> Bool
isCompleteOverlap (x1, x2, y1, y2) = (x1 - y1) * (x2 - y2) <= 0

isPartialOverlap :: (Int, Int, Int, Int) -> Bool
isPartialOverlap (x1, x2, y1, y2) = (x1 - y2) * (x2 - y1) <= 0

aaa :: String -> String
aaa xs = case drop 3 xs of
  y : ys -> y : aaa ys
  [] -> []

main :: IO ()
main =
  do
    rawInput <- readFile "data.txt"
    let (rawBoxes, commands) = seperateCommands $ splitOn [""] (lines rawInput)
    let boxes = map (dropWhile isSpace) $ transpose $ map (\x -> aaa (" "++" "++x)) rawBoxes
    print $ boxes

-- print $ sum $ map (fromEnum . isCompleteOverlap . formatData . parseLines) input
-- print $ sum $ map (fromEnum . isPartialOverlap . formatData . parseLines) input
