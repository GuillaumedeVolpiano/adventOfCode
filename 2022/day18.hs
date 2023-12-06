import           General     (retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs, getProgName)
import           Text.Regex.TDFA    (getAllTextMatches, (=~))

import           Data.Array         (Array, array, bounds, inRange, (!))
import           Data.Map           as M (Map, empty, findWithDefault, fromList,
                                          insert, lookup)
import           Data.Maybe         (isNothing)
import           Data.Sequence      as S (Seq ((:<|), (:|>)), fromList)
import           Linear.V3          (V3 (..))

type Cube = V3 Int

type Grid = Array Cube Bool

type Air = Map Cube Bool

vars = [V3 1 0 0, V3 (-1) 0 0, V3 0 1 0, V3 0 (-1) 0, V3 0 0 1, V3 0 0 (-1)]

parseLine :: String -> Cube
parseLine s = V3 (read x) (read y) (read z)
  where
    (x:y:z:_) = getAllTextMatches (s =~ "[0-9]+") :: [String]

countSides :: Grid -> Air -> Cube -> Int
countSides grid air cube = sides
  where
    neighbours = map (cube +) vars
    sides = length $ filter isFree neighbours
    gridBounds = bounds grid
    isFree c =
      not (inRange gridBounds c) ||
      (not (grid ! c) && findWithDefault False c air)

countAccum :: Grid -> Air -> Int -> Cube -> Int
countAccum grid air acc cube = acc + countSides grid air cube

airFill :: Grid -> Seq Cube -> Air -> Air
airFill grid seq air
  | null seq = air
  | otherwise = airFill grid toSee (insert s True air)
  where
    (s :<| xs) = seq
    gridBounds = bounds grid
    neighbours = map (s +) vars
    toSee = foldl (:|>) xs $ filter isEmpty neighbours
    isEmpty c =
      inRange gridBounds c && not (grid ! c) && isNothing (M.lookup c air)

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  prog <- getProgName
  let year = read $ directory =~ "[0-9]+"
      day = read $ prog =~ "[0-9]+"
  input <- retrieveInput year day args
  let cubes = map parseLine $ lines input
      minX = minimum . map (\(V3 x y z) -> x) $ cubes
      minY = minimum . map (\(V3 x y z) -> y) $ cubes
      minZ = minimum . map (\(V3 x y z) -> z) $ cubes
      maxX = maximum . map (\(V3 x y z) -> x) $ cubes
      maxY = maximum . map (\(V3 x y z) -> y) $ cubes
      maxZ = maximum . map (\(V3 x y z) -> z) $ cubes
      grid =
        array
          (V3 minX minY minZ, V3 maxX maxY maxZ)
          [ (V3 x y z, V3 x y z `elem` cubes)
          | x <- [minX .. maxX]
          , y <- [minY .. maxY]
          , z <- [minZ .. maxZ]
          ]
      edges =
        S.fromList
          [ V3 x y z
          | x <- [minX .. maxX]
          , y <- [minY .. maxY]
          , z <- [minZ .. maxZ]
          , (x == minX ||
             x == maxX || y == minY || y == maxY || z == minZ || z == maxZ) &&
              not (grid ! V3 x y z)
          ]
      nothing =
        M.fromList
          [ (V3 x y z, True)
          | x <- [minX .. maxX]
          , y <- [minY .. maxY]
          , z <- [minZ .. maxZ]
          ]
      air = airFill grid edges empty
  putStrLn "part 1"
  print $ foldl (countAccum grid nothing) 0 cubes
  putStrLn "part 2"
  print (M.lookup (V3 2 2 5) air)
  print $ foldl (countAccum grid air) 0 cubes
