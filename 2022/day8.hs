import           Data.List.Split    (splitOn)
import           InputRetrieval     (retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs)

import           Data.Array.Unboxed (Array, array, bounds, inRange, indices,
                                     (!))
import           Data.Char          (digitToInt)
import           Linear.V2          (V2 (..))

day = 8

testVisible :: Array (V2 Int) Int -> V2 Int -> Bool
testVisible treeArray pos@(V2 x y)
  | x == 0 || y == 0 || x == maxX || y == maxY = True
  | otherwise = foldl testLines False [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]
  where
    b@(_, V2 maxX maxY) = bounds treeArray
    height = treeArray ! pos
    testLines state p = state || test p (p + pos)
    test p cur
      | not (inRange b cur) = True
      | otherwise = treeArray ! cur < height && test p (p + cur)

scoreTrees :: Array (V2 Int) Int -> V2 Int -> Int
scoreTrees treeArray pos =
  product . map scoreLine $ [V2 0 1, V2 0 (-1), V2 1 0, V2 (-1) 0]
  where
    height = treeArray ! pos
    b = bounds treeArray
    scoreLine p = score p (p + pos)
    score p cur
      | not (inRange b cur) = 0
      | height <= treeArray ! cur = 1
      | otherwise = 1 + score p (p + cur)

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  let year = read . last . splitOn "/" $ directory
  input <- retrieveInput year day args
  let lined = lines input
      width = (length . head $ lined) - 1
      height = length lined - 1
      treeArray =
        array
          (V2 0 0, V2 width height)
          [ (V2 x y, digitToInt (lined !! y !! x))
          | x <- [0 .. width]
          , y <- [0 .. height]
          ] :: Array (V2 Int) Int
  putStrLn "part 1"
  print . length . filter (testVisible treeArray) $ indices treeArray
  putStrLn "part 2"
  print . maximum . map (scoreTrees treeArray) $ indices treeArray
