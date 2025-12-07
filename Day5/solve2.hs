import System.Environment (getArgs)
import Data.Bifunctor (second)
import Data.List (sort)

type Range = (Int, Int)
type ID    = Int

data FoodS = Spoiled | Fresh deriving (Show, Eq)

data DB = DB {ranges :: [Range], ids :: [ID]} deriving (Show)


parseDB :: String -> DB
parseDB input = let
    (ranges, ids) = splitOn (=="") . lines $ input
    parsedRanges  = mergeDupes . sort . parseRanges $ ranges
    parsedIDs     = parseIDs ids
  in DB parsedRanges parsedIDs
  where
    parseRanges :: [String] -> [Range]
    parseRanges = map $ both read . splitOn (=='-')
    parseIDs :: [String] -> [ID]
    parseIDs = map read
    mergeDupes :: [Range] -> [Range]
    mergeDupes (l@(ll,lh):h@(hl,hh):xs)
      | hl <= lh  =     mergeDupes ((ll,max hh lh):xs)
      | otherwise = l : mergeDupes (h:xs)
    mergeDupes el = el

lookupID :: DB -> ID -> FoodS
lookupID db id = go (ranges db) id
  where go :: [Range] -> ID -> FoodS
        go []     _      = Spoiled
        go (r:rs) id
          | inRange r id = Fresh
          | otherwise    = go rs id
        inRange :: (Int, Int) -> Int -> Bool
        inRange (low, high) v = low <= v && v <= high 

solve :: String -> Int
solve input = let
    db = parseDB input
  in sum . map getSize . ranges $ db
  where
    getSize :: Range -> Int
    getSize (low, high) = (high - low) + 1

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> readFile fname >>= print . solve
    _       -> print "Invalid number of arguments!"


splitOn :: Eq a => (a -> Bool) -> [a] -> ([a], [a])
splitOn fn [] = ([], [])
splitOn fn (x:xs)
  | fn x      = ([], xs)
  | otherwise = let (l,r) = splitOn fn xs in (x:l, r)

both :: (a->b) -> (a, a) -> (b,b)
both fn (a,b) = (fn a, fn b)
