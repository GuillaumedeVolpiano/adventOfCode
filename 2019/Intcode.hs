module Intcode
  ( Intcode
  , clearOutput
  , evalIntcode
  , execIntcode
  , execASCIIcode
  , halted
  , initialise
  , initialiseChain
  , runASCIIcode
  , runChain
  , runIntcode
  , memory
  , sendInput
  , setMemory
  , toStart
  , update
  ) where

import           Control.Monad.State (State, evalState, execState, get, gets,
                                      modify, put, runState)
import           Data.Char           (chr)
import           Data.IntMap         (IntMap, findWithDefault, fromList, insert,
                                      (!))
import           Data.List           (uncons, unfoldr)
import           Data.List.Split     (splitOn)
import           Data.Maybe          (fromJust, isNothing)

data Intcode =
  Intcode
    { memory  :: Memory
    , pointer :: Int
    , output  :: [Int]
    , input   :: [Int]
    , halted  :: Bool
    , relBase :: Int
    }
  deriving (Show, Eq)

type Machine = State Intcode Int

type Chain = State [Intcode] Int

data Mode
  = Immediate
  | Position
  | Relative

type Memory = IntMap Int

evalIntcode :: Intcode -> Int
evalIntcode = evalState execute

execIntcode :: Intcode -> [Int]
execIntcode = output . execState execute

execASCIIcode :: Intcode -> String
execASCIIcode = asciiConverter . output . execState execute

runIntcode :: Intcode -> ([Int], Intcode)
runIntcode intcode = (output ran, ran)
  where
    ran = execState execute intcode

runASCIIcode :: Intcode -> (String, Intcode)
runASCIIcode intcode = (asciiConverter . output $ ran, ran)
  where
    ran = execState execute intcode

runChain :: Int -> [Intcode] -> Int
runChain x = evalState (chain x)

execute :: Machine
execute = do
  pos <- gets pointer
  mem <- gets memory
  let rawOp = findWithDefault 0 pos mem
      opCode = mod rawOp 100
      pars = pad . unfoldr readPars $ div rawOp 100
      op
        | opCode == 1 = threeParsOp pars (+)
        | opCode == 2 = threeParsOp pars (*)
        | opCode == 3 = getInput pars
        | opCode == 4 = storeOutput pars
        | opCode == 5 = jumpOnTest pars (/= 0)
        | opCode == 6 = jumpOnTest pars (== 0)
        | opCode == 7 = threeParsOp pars (testOp (<))
        | opCode == 8 = threeParsOp pars (testOp (==))
        | opCode == 9 = modRelBase pars
        | opCode == 99 = end
        | otherwise =
          error
            ("opCode not found: " ++
             show opCode ++ "rawOpCode: " ++ show rawOp ++ "\n" ++ show mem)
  op

-- Machine routines
end :: Machine
end = do
  modify (setHalted True)
  gets ((! 0) . memory)

getInput :: [Mode] -> Machine
getInput modes = do
  pos <- gets pointer
  mem <- gets memory
  inp <- gets firstInput
  rb <- gets relBase
  let m = head modes
      np = writePos m rb (pos + 1) mem
      todo
        | isNothing inp = return (-1)
        | otherwise = do
          modify unloadInput
          modify (update np (fromJust inp))
          modify (movePos (pos + 2))
          execute
  todo

jumpOnTest :: [Mode] -> (Int -> Bool) -> Machine
jumpOnTest modes test = do
  pos <- gets pointer
  mem <- gets memory
  rb <- gets relBase
  let m1 = head modes
      m2 = modes !! 1
      v1 = readPos m1 rb (pos + 1) mem
      v2 = readPos m2 rb (pos + 2) mem
      newPos
        | test v1 = v2
        | otherwise = pos + 3
  modify (movePos newPos)
  execute

modRelBase :: [Mode] -> Machine
modRelBase modes = do
  pos <- gets pointer
  mem <- gets memory
  rb <- gets relBase
  let m = head modes
      nv = readPos m rb (pos + 1) mem
  modify (updateRelBase nv)
  modify (movePos (pos + 2))
  execute

storeOutput :: [Mode] -> Machine
storeOutput modes = do
  pos <- gets pointer
  mem <- gets memory
  rb <- gets relBase
  let m = head modes
      np = readPos m rb (pos + 1) mem
  modify (addOutput np)
  modify (movePos (pos + 2))
  execute

threeParsOp :: [Mode] -> (Int -> Int -> Int) -> Machine
threeParsOp modes op = do
  pos <- gets pointer
  mem <- gets memory
  rb <- gets relBase
  let m1 = head modes
      m2 = modes !! 1
      m3 = modes !! 2
      v1 = readPos m1 rb (pos + 1) mem
      v2 = readPos m2 rb (pos + 2) mem
      np = writePos m3 rb (pos + 3) mem
  modify (update np (op v1 v2))
  modify (movePos (pos + 4))
  execute

-- intcode functions
addOutput :: Int -> Intcode -> Intcode
addOutput val intcode = intcode {output = val : output intcode}

clearOutput :: Intcode -> Intcode
clearOutput intcode = intcode {output = []}

firstInput :: Intcode -> Maybe Int
firstInput intcode
  | isNothing unconsed = Nothing
  | otherwise = fst <$> unconsed
  where
    unconsed = uncons . input $ intcode

movePos :: Int -> Intcode -> Intcode
movePos pos intcode = intcode {pointer = pos}

setHalted :: Bool -> Intcode -> Intcode
setHalted isHalted intcode = intcode {halted = isHalted}

setMemory :: Int -> Int -> Intcode -> Intcode
setMemory pos val intcode = intcode {memory = insert pos val . memory $ intcode}

toStart :: Intcode -> Intcode
toStart intcode = intcode {pointer = 0, output = [], halted = False}

unloadInput :: Intcode -> Intcode
unloadInput intcode = intcode {input = tail . input $ intcode}

update :: Int -> Int -> Intcode -> Intcode
update pos val intcode = intcode {memory = insert pos val . memory $ intcode}

updateRelBase :: Int -> Intcode -> Intcode
updateRelBase x intcode = intcode {relBase = x + relBase intcode}

-- mode functions
readPos :: Mode -> Int -> Int -> Memory -> Int
readPos Immediate _ p mem = findWithDefault 0 p mem
readPos Position _ p mem  = findWithDefault 0 (findWithDefault 0 p mem) mem
readPos Relative rb p mem = findWithDefault 0 (rb + findWithDefault 0 p mem) mem

writePos :: Mode -> Int -> Int -> Memory -> Int
writePos Position _ p  = findWithDefault 0 p
writePos Relative rb p = (+ rb) . findWithDefault 0 p

-- Pointer functions
readPars :: Int -> Maybe (Mode, Int)
readPars rawOp
  | rawOp == 0 = Nothing
  | mod rawOp 10 == 0 = Just (Position, div rawOp 10)
  | mod rawOp 10 == 1 = Just (Immediate, div rawOp 10)
  | mod rawOp 10 == 2 = Just (Relative, div rawOp 10)

pad :: [Mode] -> [Mode]
pad modes
  | length modes == 3 = modes
  | otherwise = pad $ modes ++ [Position]

-- chain routines
chain :: Int -> Chain
chain val = do
  machines <- get
  let result = head . output
      nextStep
        | all halted machines = return (result . last $ machines)
        | otherwise = do
          let cascaded =
                scanl1
                  cascade
                  ((execState execute . sendInput val $ first) : rest)
          put cascaded
          chain (result . last $ cascaded)
        where
          Just (first, rest) = uncons machines
          cascade a = execState execute . sendInput (result a)
  nextStep

-- Initialisation
initialise :: String -> Intcode
initialise instructions = Intcode (fromList (zip [0 ..] parsed)) 0 [] [] False 0
  where
    parsed = map read . splitOn "," . filter (/= '\n') $ instructions

initialiseChain :: String -> [Int] -> [Intcode]
initialiseChain instructions = map startMachine
  where
    startMachine x = execState execute . sendInput x . initialise $ instructions

sendInput :: Int -> Intcode -> Intcode
sendInput val intcode = intcode {input = val : input intcode}

--helpers
testOp :: (Int -> Int -> Bool) -> Int -> Int -> Int
testOp test v1 v2
  | test v1 v2 = 1
  | otherwise = 0

asciiConverter :: [Int] -> String
asciiConverter = reverse . map chr
