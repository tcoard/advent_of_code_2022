{-# language TypeApplications #-}
import Data.List
import Data.List.Split

main :: IO ()
main =
    do  numStrs <- readFile "../data/input1.txt"
        let calories = map (sum . map (read @Int) . lines) $ splitOn "\n\n" numStrs
        print $ maximum calories  -- part 1
        print $ sum . take 3 . reverse $ sort calories  -- part 2
