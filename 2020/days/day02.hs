{-# LANGUAGE OverloadedStrings #-}

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import Data.Void

type Parser = Parsec Void String
type Password = (Int, Int, Char, String)

main = do
    content <- readFile "inputs/day02.txt" 
    let passwords = lines $ content
    print (part1 passwords)
    print (part2 passwords)

part1 :: [String] -> Int
part1 psws = countTrues $ map (\ x -> handleSuccess checkPassword $ parseSeq mySequence x) psws


part2 :: [String] -> Int
part2 psws = countTrues $ map (\ x -> handleSuccess checkPassword2 $ parseSeq mySequence x ) psws 

countTrues :: [Bool] -> Int
countTrues = foldr (\l r -> if l == True then r + 1 else r) 0

checkPassword :: Password -> Bool
checkPassword (min, max, pol, toCheck) = (inRange min max) $ length $ filter (\x -> x == pol) toCheck

checkPassword2 :: Password -> Bool
checkPassword2 (ind1, ind2, pol, toCheck) = (matchPol toCheck (ind1-1) pol) /= (matchPol toCheck (ind2-1) pol)

matchPol :: String -> Int -> Char -> Bool
matchPol toCheck ind pol = (toCheck !! ind) == pol

inRange :: Int -> Int -> Int -> Bool
inRange min max x = x >= min && x <= max

mySequence :: Parser Password
mySequence = 
    (,,,) <$> L.decimal <* char '-'
         <*> L.decimal <* char ' '
         <*> alphaNumChar <* char ':' <* char ' '
         <*> some alphaNumChar

parseSeq :: Parser Password -> String -> Either (ParseErrorBundle String Void) Password 
parseSeq p s = parse p "init" s

handleSuccess :: (Password -> Bool) -> Either l Password -> Bool
handleSuccess f e =
    case e of 
      Left _ -> False
      Right psw -> f psw
