{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe (mapMaybe)
import Text.Read

seperateBoxesCommands :: [[String]] -> ([String], [String])
seperateBoxesCommands [rawBoxes, commandSentences] = (init rawBoxes, commandSentences)

parseColumnContents :: String -> String
parseColumnContents xs = case drop 3 xs of
  y : ys -> y : parseColumnContents ys
  [] -> []

getIntsFromCommandSentences :: String -> [Int]
getIntsFromCommandSentences = mapMaybe readMaybe . words

seperateCommands :: [Int] -> (Int, Int, Int)
seperateCommands [amount, from, to] = (amount, from, to)

moveBoxesOneAtATime :: [String] -> (Int, Int, Int) -> [String]
moveBoxesOneAtATime boxes (amount, from, to) = secondLeft ++ ((reverse items) ++ toBox) : secondRight
  where
    (secondLeft, items, toBox, secondRight) = splitBoxes boxes (amount, from, to)

moveBoxesMultiAtATime :: [String] -> (Int, Int, Int) -> [String]
moveBoxesMultiAtATime boxes (amount, from, to) = secondLeft ++ (items ++ toBox) : secondRight
  where
    (secondLeft, items, toBox, secondRight) = splitBoxes boxes (amount, from, to)

splitBoxes :: [String] -> (Int, Int, Int) -> ([String], String, String, [String])
splitBoxes boxes (amount, from, to) = (secondLeft, items, toBox, secondRight)
  where
    (left, fromBox : right) = splitAt from boxes
    (items, modFromBox) = splitAt amount fromBox
    (secondLeft, toBox : secondRight) = splitAt to (left ++ modFromBox : right)

getTopOfStackAtEnd :: [String] -> String
getTopOfStackAtEnd boxHistory = map head $ init $ tail boxHistory

main :: IO ()
main =
  do
    rawInput <- readFile "data.txt"
    let (rawBoxes, commandSentences) = seperateBoxesCommands $ splitOn [""] (lines rawInput)
    -- "  " if for padding to get correct spacing for the first entry
    let boxes = map (dropWhile isSpace) $ transpose $ map (\x -> parseColumnContents ("  " ++ x)) rawBoxes
    let commands = map (seperateCommands . getIntsFromCommandSentences) commandSentences
    print $ getTopOfStackAtEnd $ foldl moveBoxesOneAtATime ([""] ++ boxes ++ [""]) commands
    print $ getTopOfStackAtEnd $ foldl moveBoxesMultiAtATime ([""] ++ boxes ++ [""]) commands
