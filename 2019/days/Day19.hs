module Day19
  ( part1
  , part2
  ) where

import           Intcode (Intcode, initialise, outputIntcode, sendMultInput)

findBeam :: String -> [[Int]]
findBeam intcode =
  filter
    ((== 1) . head . outputIntcode . flip sendMultInput machine)
    [[x, y] | x <- [0 .. 49], y <- [0 .. 49]]
  where
    machine = initialise intcode

findLargeBeam :: String -> Int
findLargeBeam intcode = findXY machine (floor refinedX) (floor refinedY)
  where
    shortBeam = map (\[a, b] -> (a, b)) . findBeam $ intcode
    beamLine = map snd . filter ((== 35) . fst) $ shortBeam
    appLowSlope = (fromIntegral . minimum $ beamLine) / 35 :: Rational
    appUpSlope = (fromIntegral . maximum $ beamLine) / 35 :: Rational
    firstX = 99 * (1 + appLowSlope) / (appUpSlope - appLowSlope)
    minY = appLowSlope * firstX
    maxY = appUpSlope * firstX
    newLine =
      [ y
      | y <- [floor minY - 50 .. ceiling maxY + 50]
      , (== 1) . head . outputIntcode . sendMultInput [floor firstX, y] $
          machine
      ]
    newLowSlope = (fromIntegral . minimum $ newLine) / firstX
    newUpSlope = (fromIntegral . maximum $ newLine) / firstX
    refinedX = 99 * (1 + newLowSlope) / (newUpSlope - newLowSlope)
    refinedY = refinedX * newUpSlope
    machine = initialise intcode

makeBeamLine :: Intcode -> Int -> Int -> [Int]
makeBeamLine machine locX locY =
  [ y
  | y <- [locY - 5 .. locY + 5]
  , (== 1) . head . outputIntcode . sendMultInput [locX, y] $ machine
  ]

findXY :: Intcode -> Int -> Int -> Int
findXY machine appX appY
  | isWideEnough machine appX appY &&
      not (isWideEnough machine (appX - 1) maxY) &&
      not (isWideEnough machine (appX - 2) maxY) &&
      not (isWideEnough machine (appX - 3) maxY) = appX * 10000 + (maxY - 99)
  | otherwise = findXY machine (appX - 1) maxY
  where
    localLine = makeBeamLine machine appX appY
    maxY = maximum localLine

isWideEnough :: Intcode -> Int -> Int -> Bool
isWideEnough machine appX appY =
  (== 1) . head . outputIntcode . sendMultInput [appX + 99, maxY - 99] $ machine
  where
    maxY = maximum . makeBeamLine machine appX $ appY

part1 :: Bool -> String -> String
part1 _ = show . length . findBeam

part2 :: Bool -> String -> String
part2 _ = show . findLargeBeam
