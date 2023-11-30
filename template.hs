import InputRetrieval ( retrieveInput )
import Data.List.Split ( splitOn )
import System.Directory ( getCurrentDirectory )
import System.Environment ( getArgs )

day = 

main = do
        args <- getArgs
        directory <- getCurrentDirectory
        let year = read . last . splitOn "/" $ directory
        input <- retrieveInput year day args 
        putStrLn "part 1"
        putStrLn "part 2"
