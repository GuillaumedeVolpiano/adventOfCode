import InputRetrieval ( retrieveInput )
import Data.List.Split ( splitOn)
import System.Directory ( getCurrentDirectory )
import System.Environment ( getArgs )

import Data.List ( sortBy )

main = do
        args <- getArgs
        directory <- getCurrentDirectory
        let year = read . last . splitOn "/" $ directory
        input <- retrieveInput year 1 args 
        let calories = map (sum . map read . filter (not . null) . splitOn "\n") . splitOn "\n\n" $ input 
        putStrLn "part 1"
        print . maximum $ calories
        putStrLn "part 2"
        print . sum . take 3 . sortBy (flip compare) $ calories

