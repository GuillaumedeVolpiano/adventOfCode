module Day8
  ( part1
  , part2
  ) where

import qualified Control.Applicative        as A (empty)
import           Control.Monad              (void)
import           Control.Monad.State        (State, execState, get, modify)
import           Data.Bifunctor             (second)
import           Data.Char                  (isDigit)
import           Data.Either                (fromRight)
import           Data.Set                   as S (Set, delete, empty, filter,
                                                  insert, map, member, size)
import           Data.Text                  (Text, pack)
import           Data.Void
import           Helpers.Graph              (Pos)
import           Linear.V2                  (V2 (..))
import           Text.Megaparsec            (ParsecT, eof, optional, runParserT,
                                             takeWhileP, try, (<|>))
import           Text.Megaparsec.Char       (char, eol, string)
import qualified Text.Megaparsec.Char.Lexer as L (decimal, lexeme, space)

type Screen = Set Pos

type Parser = ParsecT Void Text (State (Bool, Screen))

space :: Parser ()
space = void (char ' ') :: Parser ()

spaceConsumer :: Parser ()
spaceConsumer = L.space space A.empty A.empty

decimal :: Parser Int
decimal = L.lexeme spaceConsumer L.decimal

rows test
  | test = 3
  | otherwise = 6

columns test
  | test = 7
  | otherwise = 50

parseInput :: Parser ()
parseInput = try parseRect <|> parseRotateR <|> parseRotateC <|> end

end :: Parser ()
end = do
  eof
  return ()

parseRect :: Parser ()
parseRect = do
  string . pack $ "rect "
  a <- decimal
  char 'x'
  b <- decimal
  optional eol
  modify . second . rect a $ b
  parseInput

parseRotateR :: Parser ()
parseRotateR = do
  string . pack $ "rotate row y="
  index <- decimal
  string . pack $ "by "
  shift <- decimal
  optional eol
  modify . rotateR index $ shift
  parseInput

parseRotateC :: Parser ()
parseRotateC = do
  string . pack $ "rotate column x="
  index <- decimal
  string . pack $ "by "
  shift <- decimal
  optional eol
  modify . rotateC index $ shift
  parseInput

rect :: Int -> Int -> Screen -> Screen
rect a b screen =
  foldr insert screen [V2 x y | x <- [0 .. (a - 1)], y <- [0 .. b - 1]]

rotateR :: Int -> Int -> (Bool, Screen) -> (Bool, Screen)
rotateR index shift (test, screen) = (test, foldr insert screen' row')
  where
    row = S.filter (\(V2 _ y) -> y == index) screen
    row' = S.map (\(V2 x y) -> V2 (mod (x + shift) (columns test)) y) row
    screen' = foldr delete screen row

rotateC :: Int -> Int -> (Bool, Screen) -> (Bool, Screen)
rotateC index shift (test, screen) = (test, foldr insert screen' column')
  where
    column = S.filter (\(V2 x _) -> x == index) screen
    column' = S.map (\(V2 x y) -> V2 x (mod (y + shift) (rows test))) column
    screen' = foldr delete screen column

render :: Bool -> Screen -> String
render test screen = unlines . fmap line $ [0 .. rows test - 1]
  where
    line y = [point (V2 x y) | x <- [0 .. columns test - 1]]
    point p
      | p `member` screen = '#'
      | otherwise = ' '

part1 :: Bool -> Text -> String
part1 test =
  show . size . snd . flip execState (test, empty) . runParserT parseInput ""

part2 :: Bool -> Text -> String
part2 test =
  render test . snd . flip execState (test, empty) . runParserT parseInput ""
