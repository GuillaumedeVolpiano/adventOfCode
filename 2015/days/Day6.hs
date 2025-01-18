module Day6
  ( part1
  , part2
  , turnOn
  , turnOff
  , toggle
  , onLights
  , OnOffLights(..)
  , dlempty
  , bdlempty
  , obdlempty
  , buildSet
  , parseInput
  ) where

import           Control.Monad               (forM_)
import           Control.Monad.ST            (ST, runST)
import           Control.Monad.State         (State, evalState, gets, modify)
import           Data.Bits                   (shiftL)
import           Data.ByteString             (ByteString)
import           Data.ByteString.UTF8        (fromString)
import           Data.Either                 (fromRight)
import           Data.IntMap.Strict          (IntMap)
import qualified Data.IntMap.Strict          as IM (filter, fromList, map,
                                                    unionWith)
import           Data.IntMultiSet            (IntMultiSet, foldOccur)
import qualified Data.IntMultiSet            as IMS (difference, fromList,
                                                     union)
import           Data.IntSet                 (IntSet)
import qualified Data.IntSet                 as IS (delete, difference, foldr,
                                                    fromList, insert, member,
                                                    size, union)
import           Data.Map                    (Map, alter, unionWith)
import qualified Data.Map                    as M (filter, fromList, map)
import           Data.Maybe                  (fromJust, isNothing)
import           Data.Set                    (Set, delete, difference, insert,
                                              intersection, member, size, union)
import qualified Data.Set                    as S (fromList, map)
import           Data.Vector.Unboxed         (unsafeFreeze)
import qualified Data.Vector.Unboxed         as V (filter, length)
import           Data.Vector.Unboxed.Mutable (MVector, unsafeModify, unsafeRead,
                                              unsafeWrite)
import qualified Data.Vector.Unboxed.Mutable as MV (foldr', replicate)
import           Data.Void                   (Void)
import           Data.Word8                  (_comma)
import           Text.Megaparsec             (Parsec, ParsecT, eof, parse,
                                              runParserT, (<|>))
import           Text.Megaparsec.Byte        (char, eol, string)
import           Text.Megaparsec.Byte.Lexer  (decimal)

data Modify
  = TurnOn [Int]
  | TurnOff [Int]
  | Toggle [Int]

data Pos =
  Pos Int Int
  deriving (Eq, Ord, Show)

class Posifiable a where
  posify :: Int -> Int -> a

instance Posifiable Pos where
  posify = Pos

instance Posifiable Int where
  posify x y = x + shiftL y 10

class LightSet f where
  buildSet ::
       (Ord a, Posifiable a) => LowerX -> LowerY -> UpperX -> UpperY -> f a
  onLights :: f a -> Int
  toggle :: Ord a => f a -> f a -> f a
  turnOn :: Ord a => f a -> f a -> f a
  turnOff :: Ord a => f a -> f a -> f a

newtype OnOffLights a =
  OnOffLights (Set a)

newtype DimLights a =
  DimLights (Map a Int)

newtype BetterOnOffLights a =
  BetterOnOffLights IntSet

newtype BetterDimLights a =
  BetterDimLights (IntMap Int)

newtype OtherBetterDimLights a =
  OtherBetterDimLights IntMultiSet

instance LightSet OnOffLights where
  buildSet lx ly ux uy =
    OnOffLights . S.fromList $ [posify x y | x <- [lx .. ux], y <- [ly .. uy]]
  turnOn (OnOffLights a) (OnOffLights b) = OnOffLights . union b $ a
  turnOff (OnOffLights a) (OnOffLights b) = OnOffLights . difference b $ a
  toggle (OnOffLights a) (OnOffLights b) = OnOffLights . foldr toggleLight b $ a
  onLights (OnOffLights a) = size a

instance LightSet BetterOnOffLights where
  buildSet lx ly ux uy =
    BetterOnOffLights . IS.fromList
      $ [posify x y | x <- [lx .. ux], y <- [ly .. uy]]
  turnOn (BetterOnOffLights a) (BetterOnOffLights b) =
    BetterOnOffLights . IS.union b $ a
  turnOff (BetterOnOffLights a) (BetterOnOffLights b) =
    BetterOnOffLights . IS.difference b $ a
  toggle (BetterOnOffLights a) (BetterOnOffLights b) =
    BetterOnOffLights . IS.foldr betterToggleLight b $ a
  onLights (BetterOnOffLights a) = IS.size a

instance LightSet DimLights where
  buildSet lx ly ux uy =
    DimLights . M.fromList
      $ [(posify x y, 1) | x <- [lx .. ux], y <- [ly .. uy]]
  turnOn (DimLights a) (DimLights b) = DimLights . unionWith (+) b $ a
  turnOff (DimLights a) (DimLights b) =
    DimLights . M.filter (>= 0) . unionWith (+) b . M.map negate $ a
  toggle (DimLights a) (DimLights b) =
    DimLights . unionWith (+) b . M.map (2 *) $ a
  onLights (DimLights b) = sum b

instance LightSet BetterDimLights where
  buildSet lx ly ux uy =
    BetterDimLights . IM.fromList
      $ [(posify x y, 1) | x <- [lx .. ux], y <- [ly .. uy]]
  turnOn (BetterDimLights a) (BetterDimLights b) =
    BetterDimLights . IM.unionWith (+) b $ a
  turnOff (BetterDimLights a) (BetterDimLights b) =
    BetterDimLights . IM.filter (>= 0) . IM.unionWith (+) b . IM.map negate $ a
  toggle (BetterDimLights a) (BetterDimLights b) =
    BetterDimLights . IM.unionWith (+) b . IM.map (2 *) $ a
  onLights (BetterDimLights b) = sum b

instance LightSet OtherBetterDimLights where
  buildSet lx ly ux uy =
    OtherBetterDimLights . IMS.fromList
      $ [posify x y | x <- [lx .. ux], y <- [ly .. uy]]
  turnOn (OtherBetterDimLights a) (OtherBetterDimLights b) =
    OtherBetterDimLights . IMS.union b $ a
  turnOff (OtherBetterDimLights a) (OtherBetterDimLights b) =
    OtherBetterDimLights . IMS.difference b $ a
  toggle (OtherBetterDimLights a) (OtherBetterDimLights b) =
    OtherBetterDimLights . IMS.union b . IMS.union a $ a
  onLights (OtherBetterDimLights a) = foldOccur (\_ o -> (o +)) 0 a

type UpperX = Int

type UpperY = Int

type LowerX = Int

type LowerY = Int

type Parser a = ParsecT Void ByteString (State a)

type ParserST a = Parsec Void ByteString a

oolempty :: OnOffLights Pos
oolempty = OnOffLights mempty

boolempty :: BetterOnOffLights Int
boolempty = BetterOnOffLights mempty

dlempty :: DimLights Pos
dlempty = DimLights mempty

bdlempty :: BetterDimLights Int
bdlempty = BetterDimLights mempty

obdlempty :: OtherBetterDimLights Int
obdlempty = OtherBetterDimLights mempty

toggleLight :: Ord a => a -> Set a -> Set a
toggleLight k s
  | k `member` s = delete k s
  | otherwise = insert k s

betterToggleLight :: Int -> IntSet -> IntSet
betterToggleLight k s
  | k `IS.member` s = IS.delete k s
  | otherwise = IS.insert k s

tokenise :: (Ord a, LightSet f) => Parser (f a) (f a -> f a -> f a)
tokenise =
  ((string . fromString $ "turn on ") >> return turnOn)
    <|> ((string . fromString $ "turn off ") >> return turnOff)
    <|> ((string . fromString $ "toggle ") >> return toggle)

tokenise' :: ParserST ([Int] -> Modify)
tokenise' =
  ((string . fromString $ "turn on ") >> return TurnOn)
    <|> ((string . fromString $ "turn off ") >> return TurnOff)
    <|> ((string . fromString $ "toggle ") >> return Toggle)

parseInput' :: ParserST [Modify]
parseInput' = parseLine' <|> (eof >> return [])

parseInput :: (Ord a, Posifiable a, LightSet f) => Parser (f a) Int
parseInput = parseLine <|> (eof >> gets onLights)

parseLine :: (Ord a, Posifiable a, LightSet f) => Parser (f a) Int
parseLine = do
  op <- tokenise
  lx <- decimal
  char _comma
  ly <- decimal
  string . fromString $ " through "
  ux <- decimal
  char _comma
  uy <- decimal
  eol
  let set = buildSet lx ly ux uy
  modify (op set)
  parseInput

parseLine' :: ParserST [Modify]
parseLine' = do
  op <- tokenise'
  lx <- decimal
  char _comma
  ly <- decimal
  string . fromString $ " through "
  ux <- decimal
  char _comma
  uy <- decimal
  eol
  (op [posify x y | x <- [lx .. ux], y <- [ly .. uy]] :) <$> parseInput'

vary :: Int -> Maybe Int -> Maybe Int
vary i Nothing
  | i > 0 = Just i
  | otherwise = Nothing
vary i (Just x)
  | i + x > 0 = Just (i + x)
  | otherwise = Nothing

solvePart1 :: [Modify] -> ST s Int
solvePart1 mods = do
  lights <- MV.replicate (2 ^ 20) False
  forM_ mods $ \mod -> do
    operate1 mod lights
  V.length . V.filter id <$> unsafeFreeze lights

solvePart2 :: [Modify] -> ST s Int
solvePart2 mods = do
  lights <- MV.replicate (2 ^ 20) 0
  forM_ mods $ \mod -> do
    operate2 mod lights
  MV.foldr' (+) 0 lights

operate1 :: Modify -> MVector s Bool -> ST s ()
operate1 (TurnOn poss) lights = do
  forM_ poss $ \pos -> do
    unsafeWrite lights pos True
operate1 (TurnOff poss) lights = do
  forM_ poss $ \pos -> do
    unsafeWrite lights pos False
operate1 (Toggle poss) lights = do
  forM_ poss $ \pos -> do
    onOff <- unsafeRead lights pos
    unsafeWrite lights pos $ not onOff

operate2 :: Modify -> MVector s Int -> ST s ()
operate2 (TurnOn poss) lights = do
  forM_ poss $ \pos -> do
    unsafeModify lights (+ 1) pos
operate2 (TurnOff poss) lights = do
  forM_ poss $ \pos -> do
    unsafeModify
      lights
      (\x ->
         if x > 1
           then x - 1
           else 0)
      pos
operate2 (Toggle poss) lights = do
  forM_ poss $ \pos -> do
    unsafeModify lights (+ 2) pos

part1 :: Bool -> ByteString -> String
part1 _ input = show solved
  where
    solved = runST $ solvePart1 mods
    mods = fromRight (error "parser failed") . parse parseInput' "Day 6" $ input

part2 :: Bool -> ByteString -> String
part2 _ input = show solved
  where
    solved = runST $ solvePart2 mods
    mods = fromRight (error "parser failed") . parse parseInput' "Day 6" $ input
