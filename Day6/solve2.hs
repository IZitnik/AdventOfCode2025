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
parseProblems input = let
    rows   = lines input
    splits = reverse . findSplits . last $ rows
    problems = transpose $ map (splitOnIndexes splits) rows
    fixed = map (fix . init) problems
    operands = map last problems
  in zipWith readProb operands fixed
  where
    readProb op = Prob (parseOp op)

fix :: [String] -> [Int]
fix = map read . transpose

parseOp (x:xs)
  | x == '+'  = Add
  | x == '*'  = Mul
  | otherwise = parseOp xs
parseOp []    = undefined

findSplits :: String -> [Int]
findSplits = foldl go [] . zip [0..]
  where go acc (i, ch) = if i == 0 then acc else case ch of
                           '*' -> (i-1):acc
                           '+' -> (i-1):acc
                           _   -> acc

splitOnIndexes :: [Int] -> String -> [String]
splitOnIndexes indexes = snd . foldl go (indexes, [""]) . zip [0..] where
  go :: ([Int], [String]) -> (Int, Char) -> ([Int], [String])
  go ([]  , a:as) (_ , ch) = ([]  , (ch:a):as)
  go (i:is, a:as) (ci, ch)
    | ci == i              = (is  , []:a:as)
    | otherwise            = (i:is, (ch:a):as)
