module Main where

{- ORMOLU_DISABLE -}
--- Day imports
import qualified Days.Day1 as Day1 (runDay)
import qualified Days.Day2 as Day2 (runDay)
import qualified Days.Day3 as Day3 (runDay)
import qualified Days.Day4 as Day4 (runDay)
import qualified Days.Day5 as Day5 (runDay)
import qualified Days.Day6 as Day6 (runDay)
import qualified Days.Day7 as Day7 (runDay)
import qualified Days.Day8 as Day8 (runDay)
import qualified Days.Day9 as Day9 (runDay)
import qualified Days.Day10 as Day10 (runDay)
import qualified Days.Day11 as Day11 (runDay)
import qualified Days.Day12 as Day12 (runDay)
import qualified Days.Day13 as Day13 (runDay)
import qualified Days.Day14 as Day14 (runDay)
import qualified Days.Day15 as Day15 (runDay)
import qualified Days.Day16 as Day16 (runDay)
import qualified Days.Day17 as Day17 (runDay)
import qualified Days.Day18 as Day18 (runDay)
import qualified Days.Day19 as Day19 (runDay)
import qualified Days.Day20 as Day20 (runDay)
import qualified Days.Day21 as Day21 (runDay)
import qualified Days.Day22 as Day22 (runDay)
import qualified Days.Day23 as Day23 (runDay)
import qualified Days.Day24 as Day24 (runDay)
import qualified Days.Day25 as Day25 (runDay)

import qualified Control.Applicative.Combinators as C (option)
import Control.Monad (forM_, unless)
import Data.List (intercalate)

--- Other imports
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Options.Applicative
import Program.RunDay (Day, Verbosity(Quiet, Timings, Verbose))

import Program.Color (withColor)
import System.Console.ANSI (Color(..))

-- Data Output
import Text.Printf (printf)

{- ORMOLU_ENABLE -}
data Days
  = AllDays
  | OneDay
      { day :: Int
      , input :: Maybe String
      }
  deriving (Show)

data Options =
  Options Days Verbosity

dayParser :: Parser Days
dayParser = (OneDay <$> day <*> input) <|> allDays
  where
    day =
      option auto $
      long "day" <>
      short 'd' <> metavar "DAY" <> help "Present the solutions for one day."
    input =
      optional $
      strOption $
      long "input" <>
      short 'i' <>
      metavar "FILE" <> help "The file to read the selected day's input from."
    allDays =
      flag' AllDays $
      long "all-days" <>
      help
        (unwords
           [ "Present solutions for all of the days of"
           , "Advent of Code, with default input file names."
           ])

optionsParser :: Parser Options
optionsParser = Options <$> dayParser <*> verbosityParser
  where
    verbosityParser :: Parser Verbosity
    verbosityParser =
      C.option Quiet $
      (flag' Verbose $
       long "verbose" <>
       short 'v' <>
       help
         (unwords
            [ "Whether to print out extra info, such as the"
            , "result of the input parser, and more detailed"
            , "error messages."
            , "Also enables timing of solutions."
            ])) <|>
      (flag' Timings $
       long "timings" <>
       short 't' <>
       help (unwords ["Whether to enable timing of the solutions."]))

days :: Map Int (Day, String)
days =
  Map.fromList . zip [1 ..] $
  [ (Day1.runDay, "input/1.txt")
  , (Day2.runDay, "input/2.txt")
  , (Day3.runDay, "input/3.txt")
  , (Day4.runDay, "input/4.txt")
  , (Day5.runDay, "input/5.txt")
  , (Day6.runDay, "input/6.txt")
  , (Day7.runDay, "input/7.txt")
  , (Day8.runDay, "input/8.txt")
  , (Day9.runDay, "input/9.txt")
  , (Day10.runDay, "input/10.txt")
  , (Day11.runDay, "input/11.txt")
  , (Day12.runDay, "input/12.txt")
  , (Day13.runDay, "input/13.txt")
  , (Day14.runDay, "input/14.txt")
  , (Day15.runDay, "input/15.txt")
  , (Day16.runDay, "input/16.txt")
  , (Day17.runDay, "input/17.txt")
  , (Day18.runDay, "input/18.txt")
  , (Day19.runDay, "input/19.txt")
  , (Day20.runDay, "input/20.txt")
  , (Day21.runDay, "input/21.txt")
  , (Day22.runDay, "input/22.txt")
  , (Day23.runDay, "input/23.txt")
  , (Day24.runDay, "input/24.txt")
  , (Day25.runDay, "input/25.txt")
  ]

performDay :: Options -> IO ()
performDay (Options d v) =
  case d of
    AllDays -> do
      results <-
        let eachDay d (dayFunc, inputFile) = do
              withColor Magenta $ putStrLn $ printf "\n***Day %02d***" d
              dayFunc v inputFile
         in sequence $ Map.mapWithKey eachDay days
      printSummary results
    OneDay {..} ->
      case days Map.!? day of
        Nothing -> putStrLn "Invalid day provided. There are 25 days in Advent."
        Just (dayFunc, inputFile) -> do
          let i' = fromMaybe inputFile input
          withColor Magenta $ putStrLn $ printf "\n***Day %02d***" day
          dayFunc v i'
          withColor Magenta $ putStrLn "************"

printSummary :: Map Int (Maybe Double, Maybe Double) -> IO ()
printSummary results = do
  putStrLn "\n************\n  Summary:  "
  let partsA = Map.mapKeys ((++ " (a)") . printf "%02d") $ fmap fst results
      partsB = Map.mapKeys ((++ " (b)") . printf "%02d") $ fmap snd results
      parts = Map.toList $ partsA <> partsB
      fails = [p | (p, Nothing) <- parts]
      fasts = [(p, t) | (p, Just t) <- parts, t < 1]
      slows = [(p, t) | (p, Just t) <- parts, t >= 1]
  putStr $ printf "\n%d parts " $ length fasts
  withColor Green $ putStr "completed in under 1 second"
  putStrLn ".\nOf the remainder:"
  unless (null fails) $ do
    putStr $ printf "  %d parts" $ length fails
    withColor Red $ putStr " failed"
    putStrLn $ ":\n    " ++ intercalate ", " fails
  unless (null slows) $ do
    putStr $ printf "  %d parts" $ length slows
    withColor Yellow $ putStr " took over 1 second to complete"
    putStrLn ":"
    forM_ slows $ \(p, t) -> putStrLn $ printf "    %s took %.2f seconds" p t

main :: IO ()
main = performDay =<< execParser opts
  where
    opts =
      info
        (optionsParser <**> helper)
        (fullDesc <> progDesc "Prints out some Advent of Code solutions.")
