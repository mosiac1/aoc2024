module Days.Day2
  ( runDay
  ) where

{- ORMOLU_DISABLE -}

import qualified Program.RunDay as R (Day, runDay)
import Data.List (tails, inits)

{- ORMOLU_ENABLE -}
runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: String -> Input
inputParser = map (map read . words) . lines

------------ TYPES ------------
type Input = [[Int]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------

normalizedDx :: [Int] -> [Int]
normalizedDx values =
  let
    dx = zipWith (-) (init values) (drop 1 values)
  in
    map (* (signum $ head dx)) dx


checkReport :: [Int] -> Bool
checkReport report =
  let
    dx = normalizedDx report
  in
    all (> 0) dx && all (<= 3) dx

checkReportWithDampener :: [Int] -> Bool
checkReportWithDampener report = any checkReport $ report:zipWith (++) (inits report) (tail $ tails report)

partA :: Input -> OutputA
partA = length . filter checkReport

------------ PART B ------------
partB :: Input -> OutputB
partB = length . filter checkReportWithDampener
