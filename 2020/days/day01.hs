import System.IO

main = do
    stringNumbers <- readFile "inputs/day01.txt" 
    let numbers = makeInteger $ lines $ stringNumbers
    print (part1 numbers)
    print (part2 numbers)
    return ()

part1 :: [Int] -> Int
part1 xs = 
    let  newList = [ (x,y, x+y) | x <- xs, y <- xs ]
     in prodFirst2 $ (!! 0) $ filter (\(x,y,sum) -> sum == 2020) $ newList

part2 :: [Int] -> Int
part2 xs = 
    let newList = [ (x,y,z, x+y+z) | x <- xs, y <- xs, z <- xs ]
     in prodFirst3 $ (!! 0) $ filter (\(x,y,z,sum) -> sum == 2020) $ newList

prodFirst2 :: (Int,Int,a) -> Int
prodFirst2 (x,y,_) = x * y

prodFirst3 :: (Int,Int,Int,a) -> Int
prodFirst3 (x,y,z,_) = x * y * z

makeInteger :: [String] -> [Int]
makeInteger stringNumbers = map read stringNumbers

