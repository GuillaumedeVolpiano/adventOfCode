import           InputRetrieval     (retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs, getProgName)
import           System.TimeIt      (timeIt)
import           Text.Regex.TDFA    (getAllTextMatches, (=~))

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  prog <- getProgName
  let year = read $ directory =~ "[0-9]+"
      day = read $ prog =~ "[0-9]+"
  input <- retrieveInput year day args
  putStrLn "part 1"
  timeIt . print $ "solution 1"
  putStrLn "part 2"
  timeIt . print $ "solution 2"
