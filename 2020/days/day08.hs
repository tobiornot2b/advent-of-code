
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import Data.Void
import Data.Set as S (Set, insert, member, empty)
import Data.Vector as V (Vector, singleton, empty, (++), (!), length, update, imap, map, filter)


data Op = Acc Int | Jmp Int | Nop Int deriving Show

{- Program: PC ACC Operations-}
data Program = Program Int Int (Vector Op) (Set Int)

data StopType a = LoopStop a | PCStop a deriving Show

type Parser = Parsec Void String

main = do
    content <- readFile "inputs/day08.txt" 
    let ops = lines $ content
    print (part1 ops) 
    print (part2 ops)
    return ()

part1 :: [String] -> StopType Int
part1 ops = calculate $ Left $ Program 0 0 (readVector ops) S.empty

part2 :: [String] -> Vector (StopType Int)
part2 ops = V.filter (\s -> case s of 
                    LoopStop _ -> False
                    PCStop _ -> True)
                $ V.map (\v -> calculate $ Left $ Program 0 0 v S.empty) 
                $ V.filter (\v -> V.length v /= 0) 
                $ let { vec = readVector ops } in V.imap (\ i x -> produceVariation x i vec) vec

produceVariation :: Op -> Int -> Vector Op -> Vector Op
produceVariation op index ops = case op of 
    Acc _ -> V.empty
    Jmp x -> V.update ops (V.singleton (index, Nop x))
    Nop x -> V.update ops (V.singleton (index, Jmp x)) 

calculate :: Either Program (StopType Program) -> StopType Int
calculate validComp = case validComp of
    Left comp -> calculate (calculateStep comp)
    Right stop -> case stop of
        LoopStop comp -> case comp of
            Program pc acc ops oldPcs ->  LoopStop acc 
        PCStop comp -> case comp of 
            Program pc acc ops oldPcs ->  PCStop acc

calculateStep :: Program -> Either Program (StopType Program)
calculateStep comp = case comp of
    Program pc acc ops oldPcs -> 
        let { op = (V.!) ops pc } 
        in if S.member pc oldPcs 
            then Right $ LoopStop $ comp
            else if pc >= V.length ops 
                then Right $ PCStop $ comp
                else Left $ doOperation pc acc oldPcs ops op

doOperation pc acc oldPcs ops op = case op of
    Acc x -> Program (pc + 1) (acc + x) ops (S.insert pc oldPcs)
    Jmp x -> Program (pc + x) acc ops (S.insert pc oldPcs)
    Nop _ -> Program (pc + 1) acc ops (S.insert pc oldPcs)

readVector :: [String] -> Vector Op
readVector ops = foldr ((V.++) . createVector) V.empty ops 

createVector :: String -> Vector Op
createVector ops = let { op = parse parser "init" ops }
                      in case op of
                          Right o -> V.singleton o
                          Left _ -> error "could not parse input"

parser :: Parser Op
parser = parseAcc <|> parseJmp <|> parseNop 

parseAcc :: Parser Op
parseAcc = Acc <$ string "acc " <*> L.signed (return ()) L.decimal

parseJmp :: Parser Op
parseJmp = Jmp <$ string "jmp " <*> L.signed (return ()) L.decimal

parseNop :: Parser Op
parseNop = Nop <$ string "nop " <*> L.signed (return ()) L.decimal