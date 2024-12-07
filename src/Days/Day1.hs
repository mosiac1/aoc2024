module Days.Day1
  ( runDay
  ) where

{- ORMOLU_DISABLE -}
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Util.Util (parseInt, headAndTail)
import Data.List (sort)
import qualified Program.RunDay as R (Day, runDay)
import Data.Maybe (fromMaybe)

{- ORMOLU_ENABLE -}
runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: String -> Input
inputParser = unzip . map (headAndTail . map parseInt . words) . lines

------------ TYPES ------------
type Input = ([Int], [Int])

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA (left, right) = sum $ map abs $ zipWith (-) (sort left) (sort right)

------------ PART B ------------
partB :: Input -> OutputB
partB (left, right) =
  let
    rightFreq = Map.fromListWith (+) [(lid, 1) | lid <- right]
  in
    sum $ map (\lid -> lid * fromMaybe 0 ((`Map.lookup` rightFreq) lid)) left
