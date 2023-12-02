import           InputRetrieval     (retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs, getProgName)
import           Text.Regex.TDFA    ((=~))

import           Data.List.Split    (chunksOf)
import           Data.Map           as M (Map, empty, findWithDefault, insert,
                                          keys, lookup)
import           Data.Maybe         (Maybe (Just, Nothing), fromJust, isNothing)
import           Linear.V2          (V2 (..))

type Pos = V2 Int

type Cave = Map Pos Bool

type Height = Int

type Rocks = [[Pos]]

type Jets = [Pos]

data State =
  State
    { cave   :: Cave
    , height :: Height
    , jets   :: Jets
    , rocks  :: Rocks
    }
  deriving (Show, Eq)

initialRocks =
  cycle
    [ [V2 x 0 | x <- [0 .. 3]]
    , [V2 x y | x <- [0 .. 2], y <- [0 .. 2], x == 1 || y == 1]
    , [V2 x y | x <- [0 .. 2], y <- [0 .. 2], x == 2 || y == 0]
    , [V2 0 y | y <- [0 .. 3]]
    , [V2 x y | x <- [0, 1], y <- [0, 1]]
    ]

lottaRocks = 1000000000000

fallRock :: State -> State
fallRock state = State newCave newHeight newJets rs
  where
    (rock:rs) = rocks state
    startPos = map (\a -> a + V2 2 (3 + height state)) rock
    curCave = cave state
    newCave = foldl (\a b -> insert b True a) curCave movedRock
    newHeight =
      max (height state) ((maximum . map (\(V2 _ y) -> y) $ movedRock) + 1)
    (movedRock, newJets) = jetFall (startPos, jets state)
    jetFall (s, j:js)
      | isNothing . fall . jet j $ s = (jet j s, js)
      | otherwise = jetFall (fromJust . fall . jet j $ s, js)
    jet j s
      | canjet = jetted
      | otherwise = s
      where
        jetted = map (+ j) s
        canjet =
          all (\(V2 x _) -> x >= 0) jetted &&
          all (\(V2 x _) -> x <= 6) jetted &&
          all (\p -> isNothing . M.lookup p $ curCave) jetted
    fall s
      | canFall = Just fell
      | otherwise = Nothing
      where
        fell = map (+ V2 0 (-1)) s
        canFall =
          all (\p -> isNothing . M.lookup p $ curCave) fell &&
          all (\(V2 _ y) -> y >= 0) fell

findPattern :: [State] -> Int -> Int
findPattern states jetLength = length potPattern + 1
  where
    pruned = drop 1000 states
    testRock = head . rocks . head $ pruned
    testJets = take jetLength . jets . head $ pruned
    potPattern =
      takeWhile
        (\x ->
           (head . rocks $ x) /= testRock || take jetLength (jets x) /= testJets) .
      tail $
      pruned

patternHeight :: [State] -> Int -> Int
patternHeight states patLength =
  height (states !! (1000 + patLength)) - height (states !! 1000)

predictHeight :: [State] -> Int -> Int -> Int
predictHeight states jetLength numRocks = prediction
  where
    patL = findPattern states jetLength
    patH = patternHeight states patL
    toFall = numRocks - 1000
    (times, remainder) = divMod toFall patL
    supp = height (states !! (1000 + remainder))
    prediction = times * patH + supp

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  prog <- getProgName
  let year = read $ directory =~ "[0-9]+"
      day = read $ prog =~ "[0-9]+"
  input <- retrieveInput year day args
  let jets =
        cycle .
        map
          (\x ->
             if x == '<'
               then V2 (-1) 0
               else V2 1 0) $
        (input =~ "[<>]+")
      jetCycle = length (input =~ "[<>]+" :: String)
      initialState = State empty 0 jets initialRocks
      twentytwentytwo = take 2023 . iterate fallRock $ initialState
      afew = take 2 . iterate fallRock $ initialState
      alot = take 10000 . iterate fallRock $ initialState
  putStrLn "part 1"
  print . height . last $ twentytwentytwo
  putStrLn "part 2"
  print $ predictHeight alot jetCycle lottaRocks