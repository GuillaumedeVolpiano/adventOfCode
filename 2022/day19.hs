import           General     (retrieveInput)
import           System.Directory   (getCurrentDirectory)
import           System.Environment (getArgs, getProgName)
import           Text.Regex.TDFA    (getAllTextMatches, (=~))

import           Data.Maybe         (Maybe (Just, Nothing), fromJust, mapMaybe)
import           Data.Set           as St (empty)
import           Search             (dfsBest)

import           Debug.Trace

data Blueprint =
  Blueprint
    { index        :: Int
    , needOre      :: Int
    , needClay     :: Int
    , needObsidian :: (Int, Int)
    , needGeode    :: (Int, Int)
    }
  deriving (Show)

data Robots =
  Robots
    { oreBot      :: Int
    , clayBot     :: Int
    , obsidianBot :: Int
    , geodeBot    :: Int
    }
  deriving (Show)

data State =
  State
    { blueprint :: Blueprint
    , robots    :: Robots
    , stock     :: Stocks
    , time      :: Time
    }
  deriving (Show)

data Stocks =
  Stocks
    { ore      :: Ore
    , clay     :: Clay
    , obsidian :: Obsidian
    , geode    :: Geode
    }
  deriving (Show)

type Ore = Int

type Clay = Int

type Obsidian = Int

type Geode = Int

type Time = Int

instance Ord State where
  compare a b = compare (keyVal a) (keyVal b)

instance Eq State where
  a == b = keyVal a == keyVal b

initialBots = Robots 1 0 0 0

initialStocks = Stocks 0 0 0 0

parseLine :: String -> Blueprint
parseLine s = Blueprint i ore c (ob1, ob2) (g1, g2)
  where
    (i:ore:c:ob1:ob2:g1:g2:_) = map read $ getAllTextMatches (s =~ "[0-9]+")

-- Hashable and comparable representation of a state.
keyVal :: State -> [Int]
keyVal state =
  [ time state
  , oreBot robs
  , clayBot robs
  , obsidianBot robs
  , geodeBot robs
  , ore st
  , clay st
  , obsidian st
  , geode st
  ]
  where
    robs = robots state
    st = stock state

-- estimate the maximum number of ore bots needed
maxBot :: Blueprint -> Int
maxBot blueprint =
  max
    (max (needOre blueprint) (needClay blueprint))
    (max (fst $ needObsidian blueprint) (fst $ needGeode blueprint))

-- build a neighbour state based on the current state and a difference.
neighbourState :: State -> Maybe [Int] -> Maybe State
neighbourState _ Nothing = Nothing
neighbourState (State b robots stocks t) (Just (l:ls)) = Just newState
  where
    newState =
      State
        b
        (neighbourRobots robots (take 4 ls))
        (neighbourStocks stocks (drop 4 ls))
        (t - l)

-- build the Robots part of the neighbour state. As we can only build one robot
-- per turn, every value of the Int list but one should be 0s.
neighbourRobots :: Robots -> [Int] -> Robots
neighbourRobots (Robots or c ob g) [dor, dc, dob, dg] =
  Robots (or + dor) (c + dc) (ob + dob) (g + dg)

-- build the Stocks part of the neighbour state. The value is increased by the
-- product of the number of bots and the time, and diminished by the quantities
-- needed to build a specific robot.
neighbourStocks :: Stocks -> [Int] -> Stocks
neighbourStocks (Stocks or c ob g) [dor, dc, dob, dg] =
  Stocks (or + dor) (c + dc) (ob + dob) (g + dg)

-- calculate the potential neighbours of a given state.
neighbours :: State -> Int -> [State]
neighbours state@(State blueprint (Robots curOreBot curClayBot curObsidianBot curGeodeBot) (Stocks curOre curClay curObsidian curGeode) curTime) curBest
  -- if there is no time left or if we can't beat the current best score even by
  -- building a new geode bot in every coming turn, just end the branch
  | curTime == 0 ||
      curGeode + curGeodeBot * curTime + div (curTime * (curTime - 1)) 2 <
      curBest = []
  -- if we can't build any bot anymore, then just gather minerals till the end.
  | null canDo = [doNothing]
  -- otherwise, explore the various possible branches.
  | otherwise = canDo
  where
    canDo =
      mapMaybe
        (neighbourState state)
        [buildOreBot, buildClayBot, buildObsidianBot, buildGeodeBot]
    -- There is no need to build an ore bot if there are only two minutes left
    -- ore if we have enough bots to produce the maximum needed amount of ore to
    -- build any bot every minute. Ore bots are built with ore.
    buildOreBot
      | curTime > oreTime + 1 && curOreBot < maxBot blueprint =
        Just
          [ oreTime
          , 1
          , 0
          , 0
          , 0
          , oreTime * curOreBot - needOre blueprint
          , oreTime * curClayBot
          , oreTime * curObsidianBot
          , oreTime * curGeodeBot
          ]
      | otherwise = Nothing
    -- There is no need to build a clay bot if there are less than four minutes left,
    -- as we would then build an obsidian bot with three minutes left, a geode
    -- bot with two minutes left and then gather one obsidian in the last
    -- minute. Clay bots are built with ore.
    buildClayBot
      | curTime > clayTime + 3 && curClayBot < snd (needObsidian blueprint) =
        Just
          [ clayTime
          , 0
          , 1
          , 0
          , 0
          , clayTime * curOreBot - needClay blueprint
          , clayTime * curClayBot
          , clayTime * curObsidianBot
          , clayTime * curGeodeBot
          ]
      | otherwise = Nothing
        -- We need at least one claybot to be able to build
    buildObsidianBot
      | curClayBot == 0 = Nothing
      | curTime > obsidianTime + 1 && curObsidianBot < snd (needGeode blueprint) =
        Just
          [ obsidianTime
          , 0
          , 0
          , 1
          , 0
          , obsidianTime * curOreBot - fst (needObsidian blueprint)
          , obsidianTime * curClayBot - snd (needObsidian blueprint)
          , obsidianTime * curObsidianBot
          , obsidianTime * curGeodeBot
          ]
      | otherwise = Nothing
    buildGeodeBot
      | curObsidianBot == 0 = Nothing
      | curTime > geodeTime =
        Just
          [ geodeTime
          , 0
          , 0
          , 0
          , 1
          , geodeTime * curOreBot - fst (needGeode blueprint)
          , geodeTime * curClayBot
          , geodeTime * curObsidianBot - snd (needGeode blueprint)
          , geodeTime * curGeodeBot
          ]
      | otherwise = Nothing
    oreTime = max (roundUpDiv (needOre blueprint - curOre) curOreBot + 1) 1
    clayTime = max (roundUpDiv (needClay blueprint - curOre) curOreBot + 1) 1
    obsidianTime =
      max
        (max
           (roundUpDiv (fst (needObsidian blueprint) - curOre) curOreBot)
           (roundUpDiv (snd (needObsidian blueprint) - curClay) curClayBot) +
         1)
        1
    geodeTime =
      max
        (max
           (roundUpDiv (fst (needGeode blueprint) - curOre) curOreBot)
           (roundUpDiv (snd (needGeode blueprint) - curObsidian) curObsidianBot) +
         1)
        1
    doNothing =
      fromJust $
      neighbourState
        state
        (Just
           [ curTime
           , 0
           , 0
           , 0
           , 0
           , curTime * curOreBot
           , curTime * curClayBot
           , curTime * curObsidianBot
           , curTime * curGeodeBot
           ])

roundUpDiv :: Int -> Int -> Int
roundUpDiv a b
  | mod a b == 0 = div a b
  | otherwise = div a b + 1

checkBest :: State -> Int -> Int
checkBest state curBest = max curBest . geode . stock $ state

explore :: Blueprint -> Int -> Int
explore blueprint availableTime =
  dfsBest [initialState] 0 neighbours checkBest empty
  where
    initialState = State blueprint initialBots initialStocks availableTime

main = do
  args <- getArgs
  directory <- getCurrentDirectory
  prog <- getProgName
  let year = read $ directory =~ "[0-9]+"
      day = read $ prog =~ "[0-9]+"
  input <- retrieveInput year day args
  let blueprints = map parseLine . lines $ input
  putStrLn "part 1"
  print . sum . map (\x -> index x * explore x 24) $ blueprints
  putStrLn "part 2"
  print . product . map (`explore` 32) $ take 3 blueprints
