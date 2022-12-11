import Data.Char
import Data.List
import Data.List.Split
import Data.Set (Set, elemAt, fromList, intersection)

getHalf :: [Char] -> Set Char
getHalf x = fromList $ take (div (length x) 2) x

-- was going to make a function, but couldn't get the type signature correct, lol
-- getUnique :: [[Char]] -> Set Char
-- getUnique x = map (\y -> getHalf y `intersection` (getHalf $ reverse y)) x

getInt :: Char -> Int
getInt x
    | isUpper x = (ord x) - 38
    | otherwise = (ord x) - 96

main :: IO ()
main =
  do
    rawInput <- readFile "../data.txt"
    let input = lines $ rawInput

    -- day 1
    print $ sum $ map (\x -> getInt $ elemAt 0 $ getHalf x `intersection` (getHalf $ reverse x)) input
    -- day 2, much more hard coded than I would like
    print $ sum $ map (\x -> getInt $ elemAt 0 $ (fromList $ x!!0) `intersection` (fromList $ x!!1) `intersection` (fromList $ x!!2)) $ chunksOf 3 input
