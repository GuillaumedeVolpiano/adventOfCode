import InputRetrieval ( retrieveInput )
import Data.List.Split ( splitOn )
import System.Directory ( getCurrentDirectory )
import System.Environment ( getArgs )

day = 6

shorten :: String -> String
shorten (a:b:c:d:xs)
    | a == b = shorten (b:c:d:xs)
    | a == c || b == c = shorten (c:d:xs)
    | a == d || b == d || c == d= shorten (d:xs)
    | otherwise = xs
main = do
        args <- getArgs
        directory <- getCurrentDirectory
        let year = read . last . splitOn "/" $ directory
        input <- retrieveInput year day args 
        let sequence = init input
        putStrLn "part 1"
        print (length sequence - (length . shorten $ sequence))
        putStrLn "part 2"
