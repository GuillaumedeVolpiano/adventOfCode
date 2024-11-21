module Day24
  ( part1
  , part2
  ) where

import           Data.Either          (fromRight)
import           Data.IntMap          as M (IntMap, adjust, assocs, elems,
                                            empty, filter, foldr, insert, keys,
                                            notMember, null, update, (!))
import           Data.List            as L (foldr, maximumBy, null, sortBy,
                                            unfoldr)
import           Data.Maybe           (catMaybes, isJust, isNothing)
import           Data.Ord             (comparing)
import           Helpers.Parsers      (Parser, alpha, nums)
import           Text.Megaparsec      (eof, many, manyTill, optional, parse,
                                       sepBy, try, (<|>))
import           Text.Megaparsec.Char (char, eol, string)

import           Debug.Trace

data Group = Group
  { system     :: System
  , units      :: Int
  , hp         :: Int
  , immunities :: [Attack]
  , weaknesses :: [Attack]
  , ap         :: Int
  , attack     :: Attack
  , initiative :: Int
  , target     :: Maybe Int
  , targeted   :: Bool
  } deriving (Show, Eq, Ord)

data System
  = Immune
  | Infection
  deriving (Show, Eq, Ord)

type Attack = String

type Field = IntMap Group

isImmune :: Group -> Bool
isImmune = (== Immune) . system

isInfection :: Group -> Bool
isInfection = not . isImmune

isEnemy :: Group -> Group -> Bool
isEnemy g
  | isImmune g = isInfection
  | otherwise = isImmune

immuneAttack :: Group -> Group -> Bool
immuneAttack g1 g2 = attack g1 `elem` immunities g2

weakAttack :: Group -> Group -> Bool
weakAttack g1 g2 = attack g1 `elem` weaknesses g2

parseArmies :: Parser [Group]
parseArmies = do
  string "Immune System:"
  eol
  im <- manyTill (parseGroup Immune) eol
  string "Infection:"
  eol
  inf <- manyTill (parseGroup Infection) eof
  return (im ++ inf)

word :: Parser (Maybe String)
word = do
  optional . char $ ' '
  w <- alpha
  char ' '
  return w

parseGroup :: System -> Parser Group
parseGroup syst = do
  Just u <- nums
  many word
  Just h <- nums
  many word
  (im, weak) <- parseImmWeak
  Just a <- nums
  Just at <- word
  many word
  Just i <- nums
  optional eol
  return $ Group syst u h im weak a at i Nothing False

parseImmWeak :: Parser ([Attack], [Attack])
parseImmWeak = do
  try parseBoth
    <|> try reversed
    <|> try parseOnlyIm
    <|> parseOnlyWeak
    <|> return ([], [])

parseBoth :: Parser ([Attack], [Attack])
parseBoth = do
  string "(immune to "
  im <- parseAttacks
  string "; weak to "
  weak <- parseAttacks
  string ") "
  many word
  return (im, weak)

reversed :: Parser ([Attack], [Attack])
reversed = do
  string "(weak to "
  weak <- parseAttacks
  string "; immune to "
  im <- parseAttacks
  string ") "
  many word
  return (im, weak)

parseOnlyIm :: Parser ([Attack], [Attack])
parseOnlyIm = do
  string "(immune to "
  im <- parseAttacks
  string ") "
  many word
  return (im, [])

parseOnlyWeak :: Parser ([Attack], [Attack])
parseOnlyWeak = do
  string "(weak to "
  weak <- parseAttacks
  string ") "
  many word
  return ([], weak)

parseAttacks :: Parser [Attack]
parseAttacks = catMaybes <$> alpha `sepBy` string ", "

buildField :: String -> Field
buildField =
  L.foldr (\g m -> M.insert (initiative g) g m) M.empty
    . fromRight []
    . parse parseArmies ""

chooseOpponent :: Field -> Field
chooseOpponent field =
  L.foldr match field
    . sortBy (comparing (ep . (!) field) `mappend` compare)
    . keys
    $ field
  where
    match k f
      | L.null (enemies k f) = f
      | otherwise =
        adjust (setTargeted True) (chosen k f)
          . adjust (setTarget . Just . chosen k $ f) k
          $ f
    chosen k =
      initiative
        . maximumBy
            (comparing (damage (field ! k))
               `mappend` comparing ep
               `mappend` comparing initiative)
        . enemies k
    enemies k =
      elems
        . M.filter
            (\x ->
               isEnemy (field ! k) x
                 && not (targeted x)
                 && not (immuneAttack (field ! k) x))

setTargeted :: Bool -> Group -> Group
setTargeted b g = g {targeted = b}

setTarget :: Maybe Int -> Group -> Group
setTarget t g = g {target = t}

doRound :: Field -> Field
doRound field =
  M.foldr fight field
    . chooseOpponent
    . fmap (setTarget Nothing . setTargeted False)
    $ field

fight :: Group -> Field -> Field
fight group field
  | initiative group `M.notMember` field || isNothing (target group) = field
  | otherwise = update (hurt curGroup) targ field
  where
    curGroup = (!) field . initiative $ group
    Just targ = target group

damage :: Group -> Group -> Int
damage attacking defending
  | immuneAttack attacking defending = 0
  | weakAttack attacking defending = 2 * ep attacking
  | otherwise = ep attacking

hurt :: Group -> Group -> Maybe Group
hurt attacking defending
  | dam >= units defending * hp defending = Nothing
  | otherwise =
    Just defending {units = units defending - div dam (hp defending)}
  where
    dam = damage attacking defending

ep :: Group -> Int
ep g = units g * ap g

combat :: Field -> Field
combat field
  | M.null (M.filter isImmune field)
      || M.null (M.filter isInfection field)
      || field == newField = field
  | otherwise = combat newField
  where
    newField = doRound field

boost :: Field -> Int
boost field
  | M.null (M.filter isInfection fought) = score fought
  | otherwise = boost . fmap slightBoost $ field
  where
    fought = combat field
    slightBoost g
      | isInfection g = g
      | isImmune g = g {ap = ap g + 1}

score :: Field -> Int
score = sum . fmap units . elems

part1 :: Bool -> String -> String
part1 _ = show . score . combat . buildField

part2 :: Bool -> String -> String
part2 _ = show . boost . buildField
