module Day21
  ( part1
  , part2
  ) where

import           Control.Monad             (void)
import           Data.ByteString           (ByteString)
import           Data.List                 (sortBy)
import           Data.Maybe                (catMaybes)
import           Data.Ord                  (comparing)
import           FlatParse.Basic           (anyAsciiDecimalInt, isDigit,
                                            runParser, skipSatisfy, some)
import           Helpers.Parsers.FlatParse (Parser, extract)

data Weapon =
  Weapon Cost Damage
  deriving (Show, Eq, Ord)

data Armor =
  Armor Cost Defense
  deriving (Show, Eq, Ord)

data Ring =
  Ring Cost Damage Defense
  deriving (Show, Eq, Ord)

data Character =
  Character HP Damage Defense
  deriving (Show, Eq, Ord)

type Cost = Int

type Damage = Int

type Defense = Int

type HP = Int

weapons :: [Weapon]
weapons = [Weapon 8 4, Weapon 10 5, Weapon 25 6, Weapon 40 7, Weapon 74 8]

armors :: [Armor]
armors =
  [Armor 0 0, Armor 13 1, Armor 31 2, Armor 53 3, Armor 75 4, Armor 102 5]

rings :: [Ring]
rings =
  [ Ring 0 0 0
  , Ring 25 1 0
  , Ring 50 2 0
  , Ring 100 3 0
  , Ring 20 0 1
  , Ring 40 0 2
  , Ring 80 0 3
  ]

consume :: Parser ()
consume = void . some $ skipSatisfy (not . isDigit)

parseInput :: Parser Character
parseInput = do
  consume
  hp <- anyAsciiDecimalInt
  consume
  damage <- anyAsciiDecimalInt
  consume
  Character hp damage <$> anyAsciiDecimalInt

win :: Character -> Character -> Bool
win boss@(Character bossHP bossDamage bossArmor) me@(Character meHP meDamage meArmor) =
  (bossHP `div` meHit) < meHP `div` bossHit
    || (bossHP `div` meHit == meHP `div` bossHit && meHP `mod` bossHit /= 0)
    || (bossHP `div` meHit == (meHP `div` bossHit) + 1
          && bossHP `mod` meHit == 0)
  where
    meHit = max 1 $ meDamage - bossArmor
    bossHit = max 1 $ bossDamage - meArmor

makeMe :: Weapon -> Armor -> Ring -> Ring -> Maybe (Cost, Character)
makeMe (Weapon cw aw) (Armor ca ad) r1@(Ring cr1 ar1 dr1) r2@(Ring cr2 ar2 dr2)
  | r1 == r2 = Nothing
  | otherwise =
    Just (cw + ca + cr1 + cr2, Character 100 (aw + ar1 + ar2) (ad + dr1 + dr2))

makeMes :: [(Int, Character)]
makeMes = catMaybes $ makeMe <$> weapons <*> armors <*> rings <*> rings

lowerWin :: Character -> Int
lowerWin boss = fst . minimum . filter (win boss . snd) $ makeMes

maxLose :: Character -> Int
maxLose boss = fst . maximum . filter (not . win boss . snd) $ makeMes

part1 :: Bool -> ByteString -> String
part1 _ = show . lowerWin . extract . runParser parseInput

part2 :: Bool -> ByteString -> String
part2 _ = show . maxLose . extract . runParser parseInput
