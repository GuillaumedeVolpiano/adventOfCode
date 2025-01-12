module Day15
  ( part1
  , moveBot
  , runBot
  , runBotLarge
  , part2
  ) where

import           Control.Monad            (void)
import           Control.Monad.State.Lazy (State, get, put, runState)
import           Data.Bifunctor           (bimap, first, second)
import           Data.ByteString          (ByteString, foldl', pack)
import qualified Data.ByteString          as B (concat)
import           Data.Either              (fromRight)
import           Data.List.Split          (chunksOf)
import           Data.Maybe               (fromJust, isNothing)
import           Data.Set                 as S (Set, delete, empty, findMax,
                                                foldr, fromList, insert, map,
                                                member, notMember)
import           Data.Void                (Void)
import           Data.Word                (Word8)
import           Data.Word8               (_circum, _greater, _less,
                                           _numbersign, _v)
import           Helpers.Graph            (Pos, east, north, origin, south,
                                           west)
import           Linear.V2                (V2 (..))
import           Text.Megaparsec          (ParsecT, eof, manyTill, optional,
                                           runParserT, (<|>))
import           Text.Megaparsec.Byte     (char, eol, printChar)

data ParserState = ParserState
  { botPos   :: Pos
  , bs       :: Boulders
  , parsePos :: Pos
  , wallsSet :: Walls
  } deriving (Show)

data Sokoban = Sokoban
  { pos      :: Pos
  , boulders :: Boulders
  , walls    :: Walls
  }

data SokobanLarge = SokobanLarge
  { posLarge      :: Pos
  , bouldersLeft  :: Boulders
  , bouldersRight :: Boulders
  , wallsLarge    :: Walls
  }

instance Show Sokoban where
  show (Sokoban p boulds ws) =
    unlines . chunksOf (mx + 1)
      $ [render (V2 x y) | y <- [0 .. my], x <- [0 .. mx]]
    where
      V2 mx my = findMax ws
      render p'
        | p' == p = '@'
        | p' `member` boulds = 'O'
        | p' `member` ws = '#'
        | otherwise = '.'

instance Show SokobanLarge where
  show (SokobanLarge p lefties righties ws) =
    unlines . chunksOf (mx + 1)
      $ [render (V2 x y) | y <- [0 .. my], x <- [0 .. mx]]
    where
      V2 mx my = findMax ws
      render p'
        | p' == p = '@'
        | p' `member` lefties = '['
        | p' `member` righties = ']'
        | p' `member` ws = '#'
        | otherwise = '.'

type Boulders = Set Pos

type Parser = ParsecT Void ByteString (State ParserState)

type Walls = Set Pos

type Dir = Pos

parseInput :: Parser ByteString
parseInput = do
  parseSokoban
  B.concat <$> manyTill (pack <$> manyTill printChar eol) eof

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
  let wallsSet'
        | c == _numbersign = insert p . wallsSet $ state
        | otherwise = wallsSet state
      p = parsePos state
      state' = state {parsePos = p + east, wallsSet = wallsSet'}
  case c of
    46 -> put state' -- period
    35 -> put state' -- numbersign
    64 -> put state' {botPos = p} -- @
    79 -> put state' {bs = insert p . bs $ state} -- O
    _  -> error (show c ++ " can't be parsed")
  optional finishLine
  parseSokoban

calcGPSSum :: Boulders -> Int
calcGPSSum = sum . S.map (\(V2 x y) -> x + 100 * y)

moveBot :: Sokoban -> Word8 -> Sokoban
moveBot state dir = state {pos = pos', boulders = boulders'}
  where
    pos'
      | pos'' `member` walls state || isNothing boulders'' = pos state
      | otherwise = pos''
    boulders'
      | isNothing boulders'' = boulders state
      | otherwise = fromJust boulders''
    dir'
      | dir == _circum = north
      | dir == _v = south
      | dir == _less = west
      | dir == _greater = east
    pos'' = pos state + dir'
    boulders''
      | pos'' `member` boulders state =
        chainMove (boulders state) pos'' dir' . walls $ state
      | otherwise = Just . boulders $ state

moveBotLarge :: SokobanLarge -> Word8 -> SokobanLarge
moveBotLarge state dir =
  state {posLarge = pos', bouldersLeft = lefties', bouldersRight = righties'}
  where
    pos'
      | pos'' `member` wallsLarge state || isNothing lr = posLarge state
      | otherwise = pos''
    lefties'
      | isNothing lr = bouldersLeft state
      | otherwise = fst . fromJust $ lr
    righties'
      | isNothing lr = bouldersRight state
      | otherwise = snd . fromJust $ lr
    dir'
      | dir == _circum = north
      | dir == _v = south
      | dir == _less = west
      | dir == _greater = east
    pos'' = posLarge state + dir'
    lr
      | pos'' `notMember` bouldersLeft state
          && pos'' `notMember` bouldersRight state =
        Just (bouldersLeft state, bouldersRight state)
      | dir `elem` [_circum, _v] && pos'' `elem` bouldersRight state =
        vChainMove
          (pos'' + west, pos'')
          dir'
          (wallsLarge state)
          (bouldersLeft state, bouldersRight state)
      | dir `elem` [_circum, _v] =
        vChainMove
          (pos'', pos'' + east)
          dir'
          (wallsLarge state)
          (bouldersLeft state, bouldersRight state)
      | otherwise =
        hChainMove
          pos''
          dir'
          (wallsLarge state)
          (bouldersLeft state, bouldersRight state)

chainMove :: Boulders -> Pos -> Dir -> Walls -> Maybe Boulders
chainMove bs bp dir ws
  | pos' `member` ws = Nothing
  | pos' `member` bs = insert pos' . delete bp <$> chainMove bs pos' dir ws
  | otherwise = Just . insert pos' . delete bp $ bs
  where
    pos' = bp + dir

hChainMove ::
     Pos -> Dir -> Walls -> (Boulders, Boulders) -> Maybe (Boulders, Boulders)
hChainMove bp dir ws (lefties, righties)
  | pos' `member` ws = Nothing
  | bp `member` lefties && pos' `member` righties =
    first (delete bp . insert pos') <$> next
  | bp `member` righties && pos' `member` lefties =
    second (delete bp . insert pos') <$> next
  | bp `member` lefties = Just (delete bp . insert pos' $ lefties, righties)
  | bp `member` righties = Just (lefties, delete bp . insert pos' $ righties)
  where
    pos' = bp + dir
    next = hChainMove pos' dir ws (lefties, righties)

vChainMove ::
     (Pos, Pos)
  -> Dir
  -> Walls
  -> (Boulders, Boulders)
  -> Maybe (Boulders, Boulders)
vChainMove (left, right) dir ws (lefties, righties)
  | left' `member` ws || right' `member` ws = Nothing
  | left' `member` lefties || left' `member` righties || right' `member` lefties =
    push <$> next
  | otherwise = Just . push $ (lefties, righties)
  where
    left' = left + dir
    right' = right + dir
    push = bimap (delete left . insert left') (delete right . insert right')
    next
      | left' `member` lefties =
        vChainMove (left', right') dir ws (lefties, righties)
      | left' `member` righties && right' `member` lefties =
        vChainMove (left' + west, left') dir ws (lefties, righties)
          >>= vChainMove (right', right' + east) dir ws
      | left' `member` righties =
        vChainMove (left' + west, left') dir ws (lefties, righties)
      | right' `member` lefties =
        vChainMove (right', right' + east) dir ws (lefties, righties)

runParser :: ByteString -> (ByteString, ParserState)
runParser =
  first (fromRight (error "parser failed"))
    . flip runState (ParserState origin empty origin empty)
    . runParserT parseInput "day15"

runBot :: ByteString -> Sokoban
runBot input = foldl' moveBot originBot moves
  where
    (moves, ParserState p boulds _ ws) = runParser input
    originBot = Sokoban p boulds ws

runBotLarge :: ByteString -> SokobanLarge
runBotLarge input = foldl' moveBotLarge originBot moves
  where
    (moves, ParserState p boulds _ ws) = runParser input
    oddDoubleWidth (V2 x y) = V2 (2 * x + 1) y
    evenDoubleWidth (V2 x y) = V2 (2 * x) y
    wsLarge =
      S.foldr
        (\p -> insert (oddDoubleWidth p) . insert (evenDoubleWidth p))
        empty
        ws
    doubleSize (p, c) = [(evenDoubleWidth p, c), (oddDoubleWidth p, c)]
    lefties = S.map evenDoubleWidth boulds
    righties = S.map oddDoubleWidth boulds
    originBot = SokobanLarge (evenDoubleWidth p) lefties righties wsLarge

part1 :: Bool -> ByteString -> String
part1 _ = show . calcGPSSum . boulders . runBot

part2 :: Bool -> ByteString -> String
part2 _ = show . calcGPSSum . bouldersLeft . runBotLarge
