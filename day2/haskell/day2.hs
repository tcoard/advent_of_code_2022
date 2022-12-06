import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Data.String

win = ["A Y", "B Z", "C X"]

tie = ["A X", "B Y", "C Z"]

gamePoints :: String -> Int
gamePoints x
  | x `elem` win = 6
  | x `elem` tie = 3
  | otherwise = 0 --lose

choicePoints :: Char -> Int
choicePoints 'X' = 1
choicePoints 'Y' = 2
choicePoints x = 3 --assume input is sanitized

shiftChoice :: Char -> Int -> Char
shiftChoice x y = shiftedChoice
  where
    choices = ['A', 'B', 'C', 'A', 'B'] --there is a cycle function I could use
    newIdx = (fromJust $ elemIndex x choices) + y
    shiftedChoice = choices !! newIdx

idk :: Char -> Int
idk 'X' = 2
idk 'Y' = 0
idk x = 1

-- I could also do this by adding ascii values
translateToPart1 :: Char -> Char
translateToPart1 'A' = 'X'
translateToPart1 'B' = 'Y'
translateToPart1 'C' = 'Z'

actualChoice :: String -> Char
actualChoice x = newChoice
  where
    outcome = last x
    opponentChoice = head x
    newChoice = shiftChoice opponentChoice (idk outcome)

main :: IO ()
main =
  do
    rawInput <- readFile "../data.txt"
    let input = lines $ rawInput
    print $ sum $ map (\x -> gamePoints x + choicePoints (last x)) input
    let part2Input = map (\x -> [head x] ++ [' '] ++ [translateToPart1 $ actualChoice x]) input -- this is inefficient
    print $ zipWith (++) input part2Input
    print $ sum $ map (\x -> gamePoints x + choicePoints (last x)) part2Input

-- let calories = map (sum . map (read @Int) . lines) $ splitOn "\n\n" numStrs
-- print $ maximum calories  -- part 1
-- print $ sum . take 3 . reverse $ sort calories  -- part 2
