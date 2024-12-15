module Day15
  ( part1
  , moveBot
  , runBot
  , runBotLarge
  , part2
  ) where

import           Control.Monad            (void)
import           Control.Monad.State.Lazy (State, get, put, runState)
import           Data.Array.IArray        as A (Array, array, bounds, (!))
import           Data.Bifunctor           (bimap, first, second)
import           Data.Either              (fromRight)
import           Data.List                (foldl')
import           Data.List.Split          (chunksOf)
import           Data.Maybe               (fromJust, isNothing)
import           Data.Set                 as S (Set, delete, empty, fromList,
                                                insert, map, member, notMember)
import           Data.Text                (Text)
import           Data.Void                (Void)
import           Helpers.Graph            (Pos, east, north, origin, south,
                                           west)
import           Linear.V2                (V2 (..))
import           Text.Megaparsec          (ParsecT, eof, manyTill, optional,
                                           runParserT, (<|>))
import           Text.Megaparsec.Char     (char, eol, printChar)

data ParserState = ParserState
  { botPos   :: Pos
  , bs       :: Boulders
  , parsePos :: Pos
  , sokoList :: [(Pos, Char)]
  } deriving (Show)

data BotState = BotState
  { pos      :: Pos
  , boulders :: Boulders
  , sokoban  :: Sokoban
  }

data BotLargeState = BotLargeState
  { posLarge      :: Pos
  , bouldersLeft  :: Boulders
  , bouldersRight :: Boulders
  , sokobanLarge  :: Sokoban
  }

instance Show BotState where
  show (BotState p boulds soko) =
    unlines . chunksOf (mx + 1)
      $ [render (V2 x y) | y <- [0 .. my], x <- [0 .. mx]]
    where
      (_, V2 mx my) = bounds soko
      render p'
        | p' == p = '@'
        | p' `member` boulds = 'O'
        | otherwise = soko ! p'

instance Show BotLargeState where
  show (BotLargeState p lefties righties soko) =
    unlines . chunksOf (mx + 1)
      $ [render (V2 x y) | y <- [0 .. my], x <- [0 .. mx]]
    where
      (_, V2 mx my) = bounds soko
      render p'
        | p' == p = '@'
        | p' `member` lefties = '['
        | p' `member` righties = ']'
        | otherwise = soko ! p'

type Boulders = Set Pos

type Parser = ParsecT Void Text (State ParserState)

type Sokoban = Array Pos Char

type Dir = Pos

parseInput :: Parser String
parseInput = do
  parseSokoban
  concat <$> manyTill (manyTill printChar eol) eof

parseSokoban :: Parser ()
parseSokoban = parseChar <|> void eol

finishLine :: Parser ()
finishLine = do
  void eol
  state <- get
  let (V2 x y) = parsePos state
  put $ state {parsePos = V2 0 (y + 1)}

parseChar :: Parser ()
parseChar = do
  c <- printChar
  state <- get
  let charVal
        | c == '#' = '#'
        | otherwise = '.'
      p = parsePos state
      sl = sokoList state
      state' = state {parsePos = p + east, sokoList = (p, charVal) : sl}
  case c of
    '.' -> put state'
    '#' -> put state'
    '@' -> put state' {botPos = p}
    'O' -> put state' {bs = insert p . bs $ state}
    _   -> error (c : " can't be parsed")
  optional finishLine
  parseSokoban

calcGPSSum :: Boulders -> Int
calcGPSSum = sum . S.map (\(V2 x y) -> x + 100 * y)

moveBot :: BotState -> Char -> BotState
moveBot state dir = state {pos = pos', boulders = boulders'}
  where
    pos'
      | sokoban state ! pos'' == '#' || isNothing boulders'' = pos state
      | otherwise = pos''
    boulders'
      | isNothing boulders'' = boulders state
      | otherwise = fromJust boulders''
    dir'
      | dir == '^' = north
      | dir == 'v' = south
      | dir == '<' = west
      | dir == '>' = east
    pos'' = pos state + dir'
    boulders''
      | pos'' `member` boulders state =
        chainMove (boulders state) pos'' dir' . sokoban $ state
      | otherwise = Just . boulders $ state

moveBotLarge :: BotLargeState -> Char -> BotLargeState
moveBotLarge state dir =
      state {posLarge = pos', bouldersLeft = lefties', bouldersRight = righties'}
  where
    pos'
      | sokobanLarge state ! pos'' == '#' || isNothing lr = posLarge state
      | otherwise = pos''
    lefties'
      | isNothing lr = bouldersLeft state
      | otherwise = fst . fromJust $ lr
    righties'
      | isNothing lr = bouldersRight state
      | otherwise = snd . fromJust $ lr
    dir'
      | dir == '^' = north
      | dir == 'v' = south
      | dir == '<' = west
      | dir == '>' = east
    pos'' = posLarge state + dir'
    lr
      | pos'' `notMember` bouldersLeft state
          && pos'' `notMember` bouldersRight state =
        Just (bouldersLeft state, bouldersRight state)
      | dir `elem` "^v" && pos'' `elem` bouldersRight state =
        vChainMove
          (pos'' + west, pos'')
          dir'
          (sokobanLarge state)
          (bouldersLeft state, bouldersRight state)
      | dir `elem` "^v" =
        vChainMove
          (pos'', pos'' + east)
          dir'
          (sokobanLarge state)
          (bouldersLeft state, bouldersRight state)
      | otherwise =
        hChainMove
          pos''
          dir'
          (sokobanLarge state)
          (bouldersLeft state, bouldersRight state)

chainMove :: Boulders -> Pos -> Dir -> Sokoban -> Maybe Boulders
chainMove bs bp dir soko
  | soko ! pos' == '#' = Nothing
  | pos' `member` bs = insert pos' . delete bp <$> chainMove bs pos' dir soko
  | otherwise = Just . insert pos' . delete bp $ bs
  where
    pos' = bp + dir

hChainMove ::
     Pos -> Dir -> Sokoban -> (Boulders, Boulders) -> Maybe (Boulders, Boulders)
hChainMove bp dir soko (lefties, righties)
  | soko ! pos' == '#' = Nothing
  | bp `member` lefties && pos' `member` righties =
    first (delete bp . insert pos') <$> next
  | bp `member` righties && pos' `member` lefties =
    second (delete bp . insert pos') <$> next
  | bp `member` lefties = Just (delete bp . insert pos' $ lefties, righties)
  | bp `member` righties = Just (lefties, delete bp . insert pos' $ righties)
  where
    pos' = bp + dir
    next = hChainMove pos' dir soko (lefties, righties)

vChainMove ::
     (Pos, Pos)
  -> Dir
  -> Sokoban
  -> (Boulders, Boulders)
  -> Maybe (Boulders, Boulders)
vChainMove (left, right) dir soko (lefties, righties)
  | soko ! left' == '#' || soko ! right' == '#' = Nothing
  | left' `member` lefties || left' `member` righties || right' `member` lefties =
    push <$> next
  | otherwise = Just . push $ (lefties, righties)
  where
    left' = left + dir
    right' = right + dir
    push = bimap (delete left . insert left') (delete right . insert right')
    next
      | left' `member` lefties =
        vChainMove (left', right') dir soko (lefties, righties)
      | left' `member` righties && right' `member` lefties =
        vChainMove (left' + west, left') dir soko (lefties, righties)
          >>= vChainMove (right', right' + east) dir soko
      | left' `member` righties =
        vChainMove (left' + west, left') dir soko (lefties, righties)
      | right' `member` lefties =
        vChainMove (right', right' + east) dir soko (lefties, righties)

runParser :: Text -> (String, ParserState)
runParser =
  first (fromRight (error "parser failed"))
    . flip runState (ParserState origin empty origin [])
    . runParserT parseInput "day15"

runBot :: Text -> BotState
runBot input = foldl' moveBot originBot moves
  where
    (moves, ParserState p boulds _ sokoL) = runParser input
    upBound = fst . head $ sokoL
    soko = array (origin, upBound) sokoL
    originBot = BotState p boulds soko

runBotLarge :: Text -> BotLargeState
runBotLarge input = foldl' moveBotLarge originBot moves
  where
    (moves, ParserState p boulds _ sokoL) = runParser input
    upBound = oddDoubleWidth . fst . head $ sokoL
    oddDoubleWidth (V2 x y) = V2 (2 * x + 1) y
    evenDoubleWidth (V2 x y) = V2 (2 * x) y
    sokoLarge = array (origin, upBound) . concatMap doubleSize $ sokoL
    doubleSize (p, c) = [(evenDoubleWidth p, c), (oddDoubleWidth p, c)]
    lefties = S.map evenDoubleWidth boulds
    righties = S.map oddDoubleWidth boulds
    originBot = BotLargeState (evenDoubleWidth p) lefties righties sokoLarge

part1 :: Bool -> Text -> String
part1 _ = show . calcGPSSum . boulders . runBot

part2 :: Bool -> Text -> String
part2 _ = show . calcGPSSum . bouldersLeft . runBotLarge
