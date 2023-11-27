import InputRetrieval ( retrieveInput )
import Data.List.Split ( splitOn )
import System.Directory ( getCurrentDirectory )
import System.Environment ( getArgs )

import Data.Char ( ord )

score1 :: String -> Int
score1 (a:_:b:_)
    | ord a == ord b - 23 = ord b - 84
    | a == 'A' && b == 'Z' = 3
    | a == 'C' && b == 'X' = 7
    | ord a < ord b - 23 = ord b - 81
    | otherwise = ord b - 87

score2 :: String -> Int
score2 (a:_:b:_)
    | b == 'Y' = ord a - 61
    | b == 'X' = mod (ord a - 66) 3 + 1
    | b == 'Z' = mod (ord a - 67) 3 + 7

main = do
        args <- getArgs
        directory <- getCurrentDirectory
        let year = read . last . splitOn "/" $ directory
        input <- retrieveInput year 2 args 
        let parsedInput = lines input
        putStrLn "part 1"
        print . sum . map score1 $ parsedInput
        putStrLn "part 2"
        print . sum . map score2 $ parsedInput
