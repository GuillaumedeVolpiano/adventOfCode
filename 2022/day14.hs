import           Data.List.Split    (splitOn)
import           General     (retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs)

import           Data.Map           (Map, fromList, insert, keys, member,
                                     notMember)
import           Linear.V2          (V2 (..))

type Pos = V2 Int

d = V2 0 1

dl = V2 (-1) 1

dr = V2 1 1

day = 14

vLine :: (Pos, Pos) -> [(Pos, Char)]
vLine (V2 a b, V2 c d)
  | a == c = map (\k -> (V2 a k, '#')) [y .. yM]
  | b == d = map (\k -> (V2 k b, '#')) [x .. xM]
  | otherwise = error "Positions do not seem to be aligned."
  where
    y = min b d
    yM = max b d
    x = min a c
    xM = max a c

fallSand :: Pos -> Int -> Map Pos Char -> Map Pos Char
fallSand pos@(V2 _ y) maxDepth cave
  | y >= maxDepth = cave
  | notMember (pos + d) cave = fallSand (pos + d) maxDepth cave
  | notMember (pos + dl) cave = fallSand (pos + dl) maxDepth cave
  | notMember (pos + dr) cave = fallSand (pos + dr) maxDepth cave
  | otherwise = insert pos 'o' cave

fallSand2 :: Pos -> Int -> Map Pos Char -> Map Pos Char
fallSand2 pos@(V2 _ y) maxDepth cave
  | y == maxDepth + 1 = insert pos 'o' cave
  | notMember (pos + d) cave = fallSand2 (pos + d) maxDepth cave
  | notMember (pos + dl) cave = fallSand2 (pos + dl) maxDepth cave
  | notMember (pos + dr) cave = fallSand2 (pos + dr) maxDepth cave
  | pos == V2 500 0 && member pos cave = cave
  | otherwise = insert pos 'o' cave

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  let year = read . last . splitOn "/" $ directory
  input <- retrieveInput year day args
  let cave =
        fromList .
        concatMap vLine .
        concatMap
          ((\x -> zip (init x) (tail x)) .
           map ((\(x:y:_) -> V2 (read x) (read y)) . splitOn ",") .
           splitOn " -> ") .
        lines $
        input
      maxDepth = maximum . map (\(V2 _ y) -> y) . keys $ cave
      sandedCave = iterate (fallSand (V2 500 0) maxDepth) cave
      sandedCave2 = iterate (fallSand2 (V2 500 0) maxDepth) cave
  putStrLn "part 1"
  print . length $ takeWhile (uncurry (/=)) (zip sandedCave (tail sandedCave))
  putStrLn "part 2"
  print $ length (takeWhile (uncurry (/=)) (zip sandedCave2 (tail sandedCave2)))
