import System.Environment (getArgs)
import Data.Sequence ( Seq(Empty, (:<|)), (<|), fromList, (|>) ) 
import Data.Foldable (toList)

solve :: String -> Int
solve = sum . map (read . findBest) . lines

findBest :: String -> String
findBest = uncurry go . both fromList . splitAt 12
  where go :: Seq Char -> Seq Char -> String
        go best Empty     = toList best
        go best (x :<| xs) = go (reduceNum (best |> x)) xs

reduceNum :: Seq Char -> Seq Char
reduceNum (x :<| y :<| xs)
  | x <  y    = y <| xs
  | otherwise = x <| reduceNum (y <| xs)
reduceNum _   = Empty

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> readFile fname >>= print . solve
    _       -> print "Invalid number of arguments!"

both :: (a->b) -> (a, a) -> (b,b)
both fn (a,b) = (fn a, fn b)

