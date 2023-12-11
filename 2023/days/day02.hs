import System.IO
import Data.Char (isDigit)
import Data.Map (Map, fromList, toList)
import qualified Data.Map as Map
import Text.Parsec
import Text.Parsec.String

data Color = Red | Green | Blue deriving (Show, Eq, Ord)
data Game = Game Int [Map Color Int] deriving (Show)

main = do
    content <- readFile "inputs/day02.txt" 
    let inputLines = lines $ content
    part1 inputLines
    part2 inputLines
    return () 
    
part1 :: [String] -> IO ()
part1 inputLines = do
    let games = map parseLine inputLines
    let validGames = map snd $ filter (\x -> fst x == True) $ checkGames games
    print $ "Part 1: " ++ (show $ sumGameNumbers validGames)
    return ()

part2 :: [String] -> IO ()
part2 inputLines = do
    let linesToUse = inputLines
    let games = map parseLine linesToUse
    let mins = map mapToMax $ map (\(Just (Game _ maps)) -> maps) games
    let products = map productOfMaxValues mins
    print $ "Part 2: " ++ (show $ sum products)
    return ()

checkGames :: [Maybe Game] -> [(Bool, Game)]
checkGames = map (\game -> case game of
    Nothing -> (False, Game 0 [])
    Just game -> (isValidGame game, game))

sumGameNumbers :: [Game] -> Int
sumGameNumbers games = sum $ map (\(Game n _) -> n) games


isValidGame :: Game -> Bool
isValidGame (Game _ maps) = all (== True) $ map (\x -> isValidRound x) maps

isValidRound :: Map Color Int -> Bool
isValidRound round = all (\(color, count) -> isValidColor color count) $ toList round

isValidColor :: Color -> Int -> Bool
isValidColor color count = case color of
    Red -> count <= 12
    Green -> count <= 13
    Blue -> count <= 14
    
productOfMaxValues :: Map Color Int -> Int
productOfMaxValues m = product $ map (\(color, count) -> count) $ toList m

mapToMax :: [Map Color Int] -> Map Color Int
mapToMax maps = fromList $ map (\(color, count) -> (color, maximum $ map (\x -> getColorValue color x) maps)) $ toList $ fromList [(Red, 0), (Green, 0), (Blue, 0)]

getColorMaps :: Map Color Int -> Map Color Int
getColorMaps m = fromList [(Red, getColorValue Red m), (Green, getColorValue Green m), (Blue, getColorValue Blue m)]

getColorValue :: Color -> Map Color Int -> Int
getColorValue color m = case Map.lookup color m of
    Just x -> x
    Nothing -> 0
    

--- PARSING ---

parseLine :: String -> Maybe Game
parseLine input = case parse parseGame "" input of
    Left err -> Nothing
    Right game -> Just game

parseGame :: Parser Game
parseGame = do
    string "Game"
    space
    gameId <- read <$> many digit
    char ':'
    spaces
    rounds <- parseRound `sepBy` (char ';')
    roundMaps <- mapM (\round -> return $ fromList round) rounds
    return $ Game gameId roundMaps

parseRound :: Parser [(Color, Int)]
parseRound = do
    spaces
    colors <- parseColor `sepBy` (char ',')
    return $ colors
    
parseColor :: Parser (Color, Int)
parseColor = do
    spaces
    count <- read <$> many digit
    spaces
    color <- parseColorName
    return (color, count)

parseColorName :: Parser Color
parseColorName =
  try (string "blue" >> return Blue)
  <|> try (string "red" >> return Red)
  <|> (string "green" >> return Green)
