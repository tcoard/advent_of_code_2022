import Data.Char
import Data.List
import Data.List.Split
import Data.Set (Set, elemAt, fromList, intersection, empty)

getHalf :: String -> Set Char
getHalf x = fromList $ take (length x `div` 2) x

getUnique :: [Char] -> Set Char
getUnique x = getHalf x `intersection` getHalf (reverse x)

getInt :: Char -> Int
getInt x
    | isUpper x = (ord x) - 38
    | otherwise = (ord x) - 96

main :: IO ()
main =
  do
    rawInput <- readFile "../data.txt"
    let input = lines $ rawInput


    let one = fromList $ ['a', 'b', 'c']
    let two = fromList $ ['d', 'b', 'c']
    let three = fromList $ ['c', 'x', 'y']
    let test = [one, two, three]
    print $ foldr intersection (head test) (tail test)


    -- day 1
    print $ sum $ map (\x -> getInt $ elemAt 0 $ getUnique x) input
    -- day 2, much more hard coded than I would like
    print $ sum $ map (\x -> getInt $ elemAt 0 $ foldr (\y -> intersection $ fromList $ y) (fromList $ head x) (tail x)) $ chunksOf 3 input
