import System.IO
import Data.List
import Data.Char
import Data.Maybe
import Data.Set (Set, fromList, toList)
import Text.Parsec
import Text.Parsec.String

data PosValue = Dot | PosNumber Int | PosChar Char deriving (Eq, Show, Ord)
data Pos = Pos (Int, Int) PosValue deriving (Show, Eq, Ord)

main :: IO ()
main = do
    stringNumbers <- readFile "inputs/day03.txt" 
    let inputLines = lines $ stringNumbers
    part1 inputLines
    print $ "Part 2: " ++ show (fst $ part2 inputLines)
    tests inputLines
    return ()
    
part1 :: [String] -> IO ()
part1 inputLines = do
    let parsedLines = map (\(index, value) -> parseLine index value) $ zip [0..] inputLines
    let checkLine = map (\pos -> filter (\posE -> isPartNumber posE parsedLines) pos) parsedLines
    print $ "Part 1: " ++ (show $ sum $ map getPosNum $ concat checkLine)
    return ()
    
getPosNum :: Pos -> Int
getPosNum (Pos _ (PosNumber num)) = num
getPosNum _ = 0

isPartNumber :: Pos -> [[Pos]] -> Bool
isPartNumber (Pos (x,y) (PosNumber num)) pos = do
    let numOffset = length $ show num
    let left = getPosLine (x-1,y) 0 pos
    let right = getPosLine (x+numOffset,y) 0 pos
    let upperLine = getPosLine (x-1, y+1) (numOffset+1) pos
    let lowerLine = getPosLine (x-1, y-1) (numOffset+1) pos
    let allPos = concat [left, right, upperLine, lowerLine]
    let includes = filterPosChar allPos
    (length includes) > 0
isPartNumber _ _ = False

filterPosChar :: [Pos] -> [Pos]
filterPosChar = filter (\(Pos _ value) -> case value of
    PosChar _ -> True
    _ -> False)

getPosLine :: (Int, Int) -> Int -> [[Pos]] -> [Pos]
getPosLine (x,y) length posss = catMaybes $ map (\coords -> getElem coords posss) $ map (\a -> (a,y)) [x..x+length]

getElem :: (Int, Int) -> [[Pos]] -> Maybe Pos
getElem _ [] = Nothing
getElem (x,y) posss = if (checkEmpty (x,y)) then case getElementAtIndex y (expandPoss posss) of
    Nothing -> Nothing
    Just poss -> case getElementAtIndex x poss of
        Nothing -> Nothing
        Just pos -> Just pos
    else
        Nothing

checkEmpty (x, y) = x >= 0 && y >= 0

getElementAtIndex :: Int -> [a] -> Maybe a
getElementAtIndex index list = listToMaybe (drop index list)

expandPoss :: [[Pos]] -> [[Pos]]
expandPoss posss = map (\poss -> concatMap (\pos -> expandPos pos) poss) posss

expandPos :: Pos -> [Pos]
expandPos (Pos (x,y) (PosNumber num)) = map (\(index, _) -> Pos (x+index, y) (PosNumber num)) $ zip [0..] [0..((length $ show num) - 1)]
expandPos pos = [pos]

parseLine :: Int -> String -> [Pos]
parseLine index input = case parse (parsePos index) "" input of
    Left err -> error "Error parsing line"
    Right pos -> pos
    
parsePos :: Int -> Parser [Pos]
parsePos index = many (try (parseDot index) <|> try (parseInt index) <|> try (parseChar index))

parseDot :: Int -> Parser Pos
parseDot column = do
    pos <- getPosition
    let index = sourceColumn pos
    char '.'
    return $ Pos (index - 1, column) Dot

parseInt :: Int -> Parser Pos
parseInt column = do
    pos <- getPosition
    let index = sourceColumn pos
    value <- PosNumber <$> (read <$> many1 digit)
    return $ Pos (index - 1, column) value


parseChar :: Int -> Parser Pos
parseChar column = do
    pos <- getPosition
    let index = sourceColumn pos
    value <- PosChar <$> satisfy (\c -> c /= '.' && not (isDigit c))
    return $ Pos (index - 1, column) value
    
test1 :: [String]
test1 = 
    [ "467..114.."
  , "...*......"
  , "..35..633."
  , "......#..."
  , "617*......"
  , ".....+.58."
  , "..592....."
  , "......755."
  , "...$.*...."
  , ".664.598.."
  ]

test2 = 
    [ 
    "400*100..."
  , "..*......."
  , "..10..633."
  ]

test3 = 
    [ 
    "..400....."
  , ".*...*...."
  , "..10..633."
  ]
tests :: [String] -> IO ()
tests inputLines = do
    let parsedLines = map (\(index, value) -> parseLine index value) $ zip [0..] test2
    -- print $ getPosLine (2,0) 2 parsedLines
    -- print $ getElem (3,2) $ expandPoss parsedLines

    let res2 =  part2 test2
    assertEqual (fst $ res2) ((400 * 100) + (400 * 10))

    let res3 =  part2 test3
    assertEqual (fst $ res3) ((400 * 633) + (400 * 10))

    assertEqual (fst $ part2 test1) 467835

    
part2 :: [String] -> (Int, String)
part2 inputLines = do
    let parsedLines = map (\(index, value) -> parseLine index value) $ zip [0..] inputLines
    let checkLine = concatMap (\pos -> map (\posE -> isStarPos posE parsedLines) pos) parsedLines
    (sum $ map fst checkLine, show $ map snd checkLine)

isStarPos :: Pos -> [[Pos]] -> (Int, String)
isStarPos (Pos (x,y) (PosChar char)) pos = if char == '*'
    then do
        let left = getPosLine (x-1,y) 0 pos
        let right = getPosLine (x+1,y) 0 pos
        let upperLine = getPosLine (x-1, y-1) 2 pos
        let lowerLine = getPosLine (x-1, y+1) 2 pos
        let allPos = concat [left, right, upperLine, lowerLine]
        let includes = filterPosNum allPos
        let numbers = fromList $ map getPosNum includes
        let sumNumbers = if length numbers == 2
            then product $ toList numbers
            else 0
        (sumNumbers, show (x,y) ++ ": " ++ show lowerLine ++ show numbers)
    else (0, "")
isStarPos _ _ = (0, "")


filterPosNum :: [Pos] -> [Pos]
filterPosNum = filter (\(Pos _ value) -> case value of
    PosNumber _ -> True
    _ -> False)
    
assertEqual :: Int -> Int -> IO ()
assertEqual a b = if a == b
    then return ()
    else error $ "Test failed: " ++ show a ++ " != " ++ show b
