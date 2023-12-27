module Day16
  ( part1
  , part2
  ) where

import           Control.Monad.State (State, evalState, get, put, runState)
import           Data.Char           (digitToInt, intToDigit)
import           Data.Sequence       as Sq (Seq ((:<|), (:|>)), drop,
                                            dropWhileL, empty, fromList, length,
                                            null, singleton, splitAt, take,
                                            (><))

data Packet =
  Packet
    { version  :: Int
    , packetID :: Int
    , value    :: Value
    }
  deriving (Show)

data Value
  = Literal Int
  | Operator (Seq Packet)
  deriving (Show)

type DecodeVal = State (Seq Char, Int) (Seq Packet)

type DecodePacket = State (Seq Char) Packet

type DecodeLit = State (Seq Char) (Seq Char)

concatOp :: Packet -> Value -> Value
concatOp p (Operator o) = Operator (p :<| o)

instance Semigroup Packet where
  (<>) p o = o {value = concatOp p . value $ o}

isLiteral :: Value -> Bool
isLiteral (Literal _) = True
isLiteral _           = False

fromLiteral :: Value -> Int
fromLiteral (Literal k) = k

fromOperator :: Value -> Seq Packet
fromOperator (Operator seq) = seq

decode :: DecodePacket
decode = do
  s <- get
  let (a :<| b :<| c :<| d :<| e :<| f :<| p@(lengthID :<| sp)) = s
      (result, remainder)
        | [d, e, f] == "100" =
          ( Packet
              (fromBin (a :<| b :<| singleton c))
              4
              (Literal . fromBin $ packLit)
          , restLit)
        | lengthID == '0' =
          ( Packet
              (fromBin (a :<| b :<| singleton c))
              (fromBin (d :<| e :<| singleton f))
              (Operator packByLength)
          , restLength)
        | lengthID == '1' =
          ( Packet
              (fromBin (a :<| b :<| singleton c))
              (fromBin (d :<| e :<| singleton f))
              (Operator packByNumber)
          , restNumber)
        where
          (packLit, restLit) = runState parseLit p
          packLength = fromBin . Sq.take 15 $ sp
          restLength = Sq.drop (15 + packLength) sp
          packByLength =
            evalState parseByLength (Sq.take packLength . Sq.drop 15 $ sp, 0)
          (packNumber, toParseNumber) = Sq.splitAt 11 sp
          (packByNumber, (restNumber, _)) =
            runState parseByNumber (toParseNumber, fromBin packNumber)
  put remainder
  return result

parseByLength :: DecodeVal
parseByLength = do
  (s, _) <- get
  let (parsed, rest) = runState decode s
      (postParsed, (final, _)) = runState parseByLength (rest, 0)
      result
        | Sq.null s = empty
        | Sq.null rest = singleton parsed
        | otherwise = parsed :<| postParsed
      toParse
        | Sq.null s || Sq.null rest = empty
        | otherwise = final
  put (toParse, 0)
  return result

parseByNumber :: DecodeVal
parseByNumber = do
  (s, n) <- get
  let (packet, unparsed) = runState decode s
      (result, remainder)
        | n == 0 = (empty, (s, 0))
        | otherwise = (packet :<| parsed, rest)
        where
          (parsed, rest) = runState parseByNumber (unparsed, n - 1)
  put remainder
  return result

parseLit :: DecodeLit
parseLit = do
  s <- get
  let (a :<| as) = s
      (result, remainder)
        | a == '1' = (Sq.take 4 as >< parsed, rest)
        | a == '0' = (Sq.take 4 as, Sq.drop 4 as)
        where
          (parsed, rest) = runState parseLit (Sq.drop 4 as)
  put remainder
  return result

fromBin :: Seq Char -> Int
fromBin = foldl (\a b -> digitToInt b + 2 * a) 0

toBin :: Int -> Seq Char
toBin 0 = singleton '0'
toBin 1 = singleton '1'
toBin x = toBin (div x 2) :|> intToDigit (mod x 2)

pad :: Seq Char -> Seq Char
pad s
  | Sq.length s == 4 = s
  | otherwise = pad ('0' :<| s)

sumVersions :: Packet -> Int
sumVersions p
  | isLiteral . value $ p = version p
  | otherwise =
    (+ version p) . sum . fmap sumVersions . fromOperator . value $ p

valPacket :: Packet -> Int
valPacket p
  | i == 0 = sum recVal
  | i == 1 = product recVal
  | i == 2 = minimum recVal
  | i == 3 = maximum recVal
  | i == 4 = fromLiteral . value $ p
  | i == 5 && fp > sp = 1
  | i == 5 = 0
  | i == 6 && fp < sp = 1
  | i == 6 = 0
  | i == 7 && fp == sp = 1
  | i == 7 = 0
  where
    i = packetID p
    recVal = fmap valPacket . fromOperator . value $ p
    (fp :<| sp :<| empty) = recVal

packets :: String -> Packet
packets =
  evalState decode . foldl (><) empty . map (pad . toBin . digitToInt) . init

part1 :: Bool -> String -> String
part1 _ = show . sumVersions . packets

part2 :: Bool -> String -> String
part2 _ = show . valPacket . packets
