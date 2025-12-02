import System.Environment (getArgs)
import Data.Text.Lazy (split, pack, unpack)

data Range a = Range a a deriving (Show, Eq)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> readFile fname >>= print . solve
    _       -> print "Invalid number of arguments!"

solve :: String -> Int
solve = sum . concatMap findInvalid . parseFile

parseFile :: String -> [Range Int]
parseFile = map (getRange . unpack) . split (== ',') . pack
  where getRange = uncurry Range . both read . splitOn (== '-')

findInvalid :: Range Int -> [Int]
findInvalid (Range low high) = filter (\x -> evenLen x && isInvalid x) [low .. high]
  where isInvalid :: Int -> Bool
        isInvalid = (== 0) . uncurry (-) . getHalves
        getHalves :: Int -> (Int, Int)
        getHalves a = let aS = show a in both read . splitAt (length aS `div` 2) $ aS
        evenLen = even . length . show

both :: (a->b) -> (a, a) -> (b,b)
both fn (a,b) = (fn a, fn b)

splitOn :: Eq a => (a -> Bool) -> [a] -> ([a], [a])
splitOn fn [] = ([], [])
splitOn fn (x:xs)
  | fn x      = ([], xs)
  | otherwise = let (l,r) = splitOn fn xs in (x:l, r)
