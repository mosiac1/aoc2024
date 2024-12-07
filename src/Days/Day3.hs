module Days.Day3
  ( runDay,
  )
where

{- ORMOLU_DISABLE -}

import qualified Program.RunDay as R (Day, runDay)
import Text.Regex.Posix ((=~))
import Util.Util (parseInt)
import Control.Monad.State (State, get, put, foldM, evalState)

{- ORMOLU_ENABLE -}
runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
readOp :: [String] -> Op
readOp (_:_:"mul":l:r:_) = Mul (parseInt l) (parseInt r)
readOp ("do()":_) = Do
readOp ("don't()":_) = Don't
readOp s = error $ "Error parsing operation - " ++ show s

inputParser :: String -> Input
inputParser input = map readOp (input =~ "((mul)\\(([0-9]+),([0-9]+)\\))|(do\\(\\))|(don't\\(\\))" :: [[String]])

------------ TYPES ------------
data Op
  = Mul Int Int
  | Do
  | Don't
  deriving (Show)

type Input = [Op]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
executeMul :: Op -> Int
executeMul (Mul l r) = l * r
executeMul _ = 0

execute :: Int -> Op -> State Bool Int
execute acc (Mul l r) = do
  isExecute <- get
  return $ acc + l * r * fromEnum isExecute
execute acc Do = do
  put True
  return acc
execute acc Don't = do
  put False
  return acc

partA :: Input -> OutputA
partA = sum . map executeMul

------------ PART B ------------
partB :: Input -> OutputB
partB ops = evalState (foldM execute 0 ops) True
