import           General            (preciseTimeIt, retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs, getProgName)
import           Text.Regex.TDFA    (getAllTextMatches, (=~))

import Data.Array.Unboxed (UArray, array, bounds, inRange, (!))
import Linear.V2 (V2(..))

data State = State Board Pos Dir Instructions deriving (Show)

type Board = UArray Pos Char
type Pos = 

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  prog <- getProgName
  let year = read $ directory =~ "[0-9]+"
      day = read $ prog =~ "[0-9]+"
  input <- retrieveInput year day args
  putStrLn "part 1"
  preciseTimeIt 3 . print $ "solution 1"
  putStrLn "part 2"
  preciseTimeIt 3 . print $ "solution 2"
