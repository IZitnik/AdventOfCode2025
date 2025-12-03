import System.Environment (getArgs)


solve :: String -> Int
solve = sum . map (read . (\(x,y)-> [x,y]) . findBest) . lines

findBest :: String -> (Char, Char)
findBest = go '0' '0'
  where go :: Char -> Char -> String -> (Char, Char)
        go l r []     = (l, r)
        go l r (x:xs)
          | x > l     = if null xs then go l x xs else go x '0' xs
          | x > r     = go l x xs
          | otherwise = go l r xs

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> readFile fname >>= print . solve
    _       -> print "Invalid number of arguments!"
