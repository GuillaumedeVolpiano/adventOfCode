import           Data.List.Split    (splitOn)
import           General     (retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs)

import           Data.Array.Unboxed as A (Array, array, bounds, inRange,
                                          indices, (!))
import           Data.Char          (ord)
import           Data.Map           as M (Map, empty, insert, notMember, (!))
import           Data.Sequence      as S (Seq ((:<|), (:|>)), null, singleton)
import           Linear.V2          (V2 (..))

type Pos = V2 Int

data State =
  State (Array Pos Char) (Seq Pos) (Map Pos Int)

day = 12

up = V2 0 (-1)

down = V2 0 1

left = V2 (-1) 0

right = V2 1 0

directions = [up, down, left, right]

bFS :: State -> Int
bFS (State elevMap queue dists)
  | S.null queue = 1000000
  | elevMap A.! q == 'E' = dist
  | otherwise = bFS (State elevMap newQueue newDists)
  where
    (q :<| qs) = queue
    dist = dists M.! q
    nextPos =
      filter (\x -> accessible x && notMember x dists) . map (q +) $ directions
    newQueue = foldl (:|>) qs nextPos
    newDists = foldl (\x y -> insert y (dist + 1) x) dists nextPos
    accessible np =
      inRange (bounds elevMap) np && heightDif (elevMap A.! q) (elevMap A.! np)
    heightDif c d
      | c == 'S' = d == 'a' || d == 'b'
      | d == 'E' = c == 'y' || c == 'z'
      | d == 'S' = True
      | otherwise = ord d - ord c <= 1

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  let year = read . last . splitOn "/" $ directory
  input <- retrieveInput year day args
  let lined = lines input
      height = length lined - 1
      width = (length . head $ lined) - 1
      elevMap =
        array
          (V2 0 0, V2 width height)
          [(V2 x y, lined !! y !! x) | x <- [0 .. width], y <- [0 .. height]] :: Array Pos Char
      borders =
        filter (\(V2 x y) -> x == 0 || y == 0 || x == width || y == height) .
        indices $
        elevMap
      startPos = head . filter (\x -> elevMap A.! x == 'S') $ borders
      allStartPos =
        filter (\x -> elevMap A.! x == 'S' || elevMap A.! x == 'a') borders
      initState = State elevMap (singleton startPos) (insert startPos 0 empty)
  putStrLn "part 1"
  print . bFS $ initState
  putStrLn "part 2"
  print .
    minimum . map (\x -> bFS (State elevMap (singleton x) (insert x 0 empty))) $
    allStartPos
