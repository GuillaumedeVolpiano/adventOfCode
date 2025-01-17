{-# LANGUAGE TupleSections #-}

module Day22
  ( part1
  , part2
  ) where

import           Control.Monad             (void)
import           Data.Bifunctor            (bimap)
import           Data.ByteString           (ByteString)
import           Data.Hashable             (Hashable, hashWithSalt)
import           Data.HashMap.Strict       (HashMap, member, (!))
import qualified Data.HashMap.Strict       as M (insert, singleton)
import           Data.HashPSQ              (HashPSQ, minView)
import qualified Data.HashPSQ              as Q (insert, singleton)
import           Data.Maybe                (mapMaybe)
import           FlatParse.Basic           (anyAsciiDecimalInt, isDigit,
                                            runParser, skipSatisfy, some)
import           Helpers.Parsers.FlatParse (Parser, extract)

data Boss = Boss
  { hpb    :: HP
  , damage :: Damage
  } deriving (Show, Eq, Ord)

data Wizard = Wizard
  { hpw      :: HP
  , mana     :: Mana
  , shield   :: Shield
  , poison   :: Poison
  , recharge :: Recharge
  , armor    :: Armor
  } deriving (Show, Eq, Ord)

instance Hashable Boss where
  hashWithSalt salt (Boss h d) = hashWithSalt salt (h, d)

instance Hashable Wizard where
  hashWithSalt salt (Wizard h m s p r a) = hashWithSalt salt (h, m, s, p, r, a)

class Hurtable a where
  hp :: a -> HP

instance Hurtable Boss where
  hp = hpb

instance Hurtable Wizard where
  hp = hpw

type HP = Int

type Damage = Int

type Mana = Int

type Shield = Int

type Poison = Int

type Recharge = Int

type Armor = Int

type Pair = (Wizard, Boss)

type WinLose = (Bool, Bool)

type Turn = (Pair -> (Pair -> Pair) -> (Pair, (Bool, Bool)))

me :: Wizard
me = Wizard 50 500 0 0 0 0

actions :: [(Int, Pair -> Pair)]
actions =
  [ (53, castMagicMissile)
  , (73, castDrain)
  , (113, castShield)
  , (173, castPoison)
  , (229, castRecharge)
  ]

update :: Pair -> Pair
update (wizard, boss) =
  ( wizard
      { mana = mana'
      , shield = max (shield wizard - 1) 0
      , poison = max (poison wizard - 1) 0
      , recharge = max (recharge wizard - 1) 0
      , armor = armor'
      }
  , boss {hpb = hp'})
  where
    hp'
      | poison wizard /= 0 = hp boss - 3
      | otherwise = hp boss
    armor'
      | shield wizard /= 0 = 7
      | otherwise = 0
    mana'
      | recharge wizard /= 0 = mana wizard + 101
      | otherwise = mana wizard

castMagicMissile :: Pair -> Pair
castMagicMissile (wizard, boss) =
  (wizard {mana = mana wizard - 53}, boss {hpb = hp boss - 4})

castDrain :: Pair -> Pair
castDrain (wizard, boss) =
  ( wizard {mana = mana wizard - 73, hpw = hp wizard + 2}
  , boss {hpb = hp boss - 2})

castShield :: Pair -> Pair
castShield (wizard, boss) =
  (wizard {mana = mana wizard - 113, shield = 6}, boss) -- should the timer be 5 ?

castPoison :: Pair -> Pair
castPoison (wizard, boss) =
  (wizard {mana = mana wizard - 173, poison = 6}, boss)

castRecharge :: Pair -> Pair
castRecharge (wizard, boss) =
  (wizard {mana = mana wizard - 229, recharge = 5}, boss)

bossAttack :: Pair -> Pair
bossAttack (wizard, boss) = (wizard {hpw = hp wizard - hurt}, boss)
  where
    hurt = max (damage boss - armor wizard) 1

doTurn :: Turn
doTurn (wizard, boss) action = ((wizard'', boss''), (win, lose))
  where
    (wizard', boss') = action . update $ (wizard, boss)
    (wizard'', boss'') = bossAttack . update $ (wizard', boss')
    win = hp boss' <= 0 || hp boss'' <= 0
    lose = hp wizard'' <= 0

hurtTurn :: Turn
hurtTurn (wizard, boss) action
  | hp wizard' <= 0 = ((wizard', boss), (False, True))
  | otherwise = doTurn (wizard', boss) action
  where
    wizard' = wizard {hpw = hp wizard - 1}

winGame :: Turn -> HashPSQ (Pair, WinLose) Int () -> HashMap Pair Int -> Int
winGame turn queue dists
  | win = usedMana
  | lost || null availableActions = winGame turn rest dists
  | otherwise = winGame turn queue' dists'
  where
    Just ((pair, (win, lost)), usedMana, _, rest) = minView queue
    curMana = mana . fst $ pair
    availableActions = filter ((< curMana) . fst) actions
    nextTurns =
      mapMaybe (consider . bimap (usedMana +) (turn pair)) availableActions
    queue' =
      foldr
        (\(doublePair, newMana) -> Q.insert doublePair newMana ())
        rest
        nextTurns
    dists' =
      foldr (\((pair, _), newMana) -> M.insert pair newMana) dists nextTurns
    consider (newMana, p@(pair, winlose))
      | not (member pair dists) || newMana < dists ! pair = Just (p, newMana)
      | otherwise = Nothing

consume :: Parser ()
consume = void . some $ skipSatisfy (not . isDigit)

parseInput :: Parser Boss
parseInput = do
  consume
  hp <- anyAsciiDecimalInt
  consume
  Boss hp <$> anyAsciiDecimalInt

play :: Turn -> Pair -> Int
play turn pair =
  winGame turn (Q.singleton (pair, (False, False)) 0 ()) (M.singleton pair 0)

part1 :: Bool -> ByteString -> String
part1 _ = show . play doTurn . (me, ) . extract . runParser parseInput

part2 :: Bool -> ByteString -> String
part2 _ = show . play hurtTurn . (me, ) . extract . runParser parseInput
