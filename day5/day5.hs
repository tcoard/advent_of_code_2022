{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}


import Data.Char
import Data.List
import Data.List.Split
import Text.Read
import Data.Maybe(mapMaybe)

seperateBoxesCommands :: [[String]] -> ([String], [String])
seperateBoxesCommands [rawBoxes, commandSentences] = (init rawBoxes, commandSentences)

parseColumnContents :: String -> String
parseColumnContents xs = case drop 3 xs of
  y : ys -> y : parseColumnContents ys
  [] -> []

getIntsFromCommandSentences:: String -> [Int]
getIntsFromCommandSentences = mapMaybe readMaybe . words

seperateCommands :: [Int] -> (Int, Int, Int)
seperateCommands [amount, from, to] = (amount, from, to)

aaa :: [String] -> (Int, Int, Int) -> [String]
aaa boxes (amount, from, to) = x
    where (x,_:ys) = splitAt (from-1) boxes --splitAt amount (boxes!!from)

main :: IO ()
main =
  do
    rawInput <- readFile "data.txt"
    let (rawBoxes, commandSentences) = seperateBoxesCommands $ splitOn [""] (lines rawInput)
    -- "  " if for padding to get correct spacing for the first entry
    let boxes = map (dropWhile isSpace) $ transpose $ map (\x -> parseColumnContents ("  "++x)) rawBoxes
    let commands = map (seperateCommands . getIntsFromCommandSentences) commandSentences
    print $ boxes
    print $ commands!!1
    print $ aaa boxes (commands!!1)
    --print $ (commands!!1)

-- print $ sum $ map (fromEnum . isCompleteOverlap . formatBoxes . parseLines) input
-- print $ sum $ map (fromEnum . isPartialOverlap . formatBoxes . parseLines) input
