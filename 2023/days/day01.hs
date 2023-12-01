import System.IO
import Data.List
import Data.Maybe

main :: IO ()
main = do
    stringNumbers <- readFile "inputs/day01.txt" 
    let inputLines = lines $ stringNumbers
    print $ part1 inputLines
    print $ part2 inputLines
    return ()
    
part1 :: [String] -> Int
part1 stringNumbers = sum $ map (getFirstAndLastNumber . removeLetters) stringNumbers

getFirstAndLastNumber :: String -> Int
getFirstAndLastNumber x = read [head x, last x]::Int

removeLetters :: String -> String
removeLetters = filter (`elem` ['1'..'9'])

part2 :: [String] -> Int
part2 stringNumbers = sum $ map sumNumbers $ map parseLine stringNumbers

parseLine :: String -> [Int]
parseLine line = catMaybes $ map parseNumber $ substrings line

sumNumbers :: [Int] -> Int
sumNumbers = \x -> sum [10 * head x,  last x]

substrings :: String -> [String]
substrings = concatMap inits . tails

parseNumber :: String -> Maybe Int
parseNumber n = case n of
    "one" -> Just 1
    "two" -> Just 2
    "three" -> Just 3
    "four" -> Just 4
    "five" -> Just 5
    "six" -> Just 6
    "seven" -> Just 7
    "eight" -> Just 8
    "nine" -> Just 9
    "1" -> Just 1
    "2" -> Just 2
    "3" -> Just 3
    "4" -> Just 4
    "5" -> Just 5
    "6" -> Just 6
    "7" -> Just 7
    "8" -> Just 8
    "9" -> Just 9
    _ -> Nothing