import           General            (customPreciseTimeIt, retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs, getProgName)
import           Text.Regex.TDFA    (getAllTextMatches, (=~))

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  prog <- getProgName
  let year = read $ directory =~ "[0-9]+"
      day = read $ prog =~ "[0-9]+"
  input <- retrieveInput year day args
  customPreciseTimeIt "part 1 CPU time" 3 . print $ "solution 1"
  customPreciseTimeIt "part 2 CPU time" 3 . print $ "solution 2"
