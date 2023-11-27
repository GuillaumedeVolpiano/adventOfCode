import InputRetrieval ( retrieveInput )
import Data.List ( sortBy )
import Data.List.Split ( splitOn)
import System.Environment ( getArgs )


main = do
        args <- getArgs
        input <- retrieveInput 2022 1 args 
        let calories = map (sum . map read . filter (not . null) . splitOn "\n") . splitOn "\n\n" $ input 
        putStrLn "part 1"
        print . maximum $ calories
        putStrLn "part 2"
        print . sum . take 3 . sortBy (flip compare) $ calories

