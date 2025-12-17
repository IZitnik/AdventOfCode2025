import System.Environment (getArgs)
import Data.List (transpose)

data Problem = Prob {probOp :: Operator, probVals :: [Int]} deriving (Show)
data Operator = Add | Mul deriving (Show, Eq)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> readFile fname >>= print . solve
    _       -> print "Invalid number of arguments!"

solve :: String -> Int
solve = sum . map solveProblem . parseProblems

solveProblem :: Problem -> Int
solveProblem (Prob Add vals) = sum vals
solveProblem (Prob Mul vals) = product vals

parseProblems :: String -> [Problem]
parseProblems = map parseProblem . transpose . map splitBySpace . lines where
  parseProblem :: [String] -> Problem
  parseProblem line = Prob (readOp $ last line) (map read $ init line)
  readOp "*" = Mul
  readOp "+" = Add

splitBySpace :: String -> [String]
splitBySpace (x:' ':xs)
  | x == ' '    = splitBySpace (x:xs)
  | otherwise   = let (h:hs) = splitBySpace xs
                  in if  null h
                    then (x:h):hs
                    else [x]:h:hs
splitBySpace (x:xs)
  | x == ' '    = splitBySpace xs
  | otherwise   = let (h:hs) = splitBySpace xs in (x:h):hs
splitBySpace "" = [[]]


