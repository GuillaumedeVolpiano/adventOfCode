import           InputRetrieval     (retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs, getProgName)
import           Text.Regex.TDFA    ((=~))

import           Data.Array         (Array, array, (!))
import           Linear.V2          (V2 (..))

rocks =
  cycle
    [ array (V2 0 0, V2 3 0) [(V2 x 0, '#') | x <- [0 .. 3]]
    , array
        (V2 0 0, V2 2 2)
        ([(V2 x y, '#') | x <- [0 .. 2], y <- [0 .. 2], x == 1 || y == 1] ++
         [(V2 x y, '.') | x <- [0 .. 2], y <- [0 .. 2], x /= 1 && y /= 1])
    , array
        (V2 0 0, V2 2 2)
        ([(V2 x y, '#') | x <- [0 .. 2], y <- [0 .. 2], x == 2 || y == 0] ++
         [(V2 x y, '.') | x <- [0, 1], y <- [1, 2]])
    , array (V2 0 0, V2 0 4) [(V2 0 y, '#') | y <- [0 .. 3]]
    , array (V2 0 0, V2 1 1) [(V2 x y, '#') | x <- [0, 1], y <- [0, 1]]
    ]

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  prog <- getProgName
  let year = read $ directory =~ "[0-9]+"
      day = read $ prog =~ "[0-9]+"
  input <- retrieveInput year day args
  putStrLn "part 1"
  putStrLn "part 2"
