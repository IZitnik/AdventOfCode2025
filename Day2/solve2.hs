import System.Environment (getArgs)

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
parseFile = map getRange . split (== ',')
  where getRange = uncurry Range . both read . splitOn (== '-')

findInvalid :: Range Int -> [Int]
findInvalid (Range low high) = filter isValid [low .. high]
  where isValid :: Int -> Bool
        isValid num =
          let strNum  = show num
              minPref = repeatedSeq strNum
          in minPref /= strNum && minPref /= ""

repeatedSeq :: String -> String
repeatedSeq input =
  let
    pi = prefixFunction input
    n  = length input
    l  = last pi
    k  = n - l
  in if k > 0 && n `mod` k == 0
    then take k input
    else ""
  where prefixFunction :: String -> [Int]
        prefixFunction s = go 1 0 (replicate n 0)
          where
            n = length s

            go :: Int -> Int -> [Int] -> [Int]
            go i j piArr
              | i == n    = piArr
              | otherwise =
                  let j'  = fallback j
                      j'' = if s !! i == s !! j' then j' + 1 else j'
                      piArr' = update piArr i j''
                  in go (i + 1) j'' piArr'
              where
                fallback 0 = 0
                fallback k
                  | s !! i == s !! k = k
                  | otherwise        = fallback (piArr !! (k - 1))
            update xs idx v = take idx xs ++ [v] ++ drop (idx + 1) xs

both :: (a->b) -> (a, a) -> (b,b)
both fn (a,b) = (fn a, fn b)

splitOn :: Eq a => (a -> Bool) -> [a] -> ([a], [a])
splitOn fn [] = ([], [])
splitOn fn (x:xs)
  | fn x      = ([], xs)
  | otherwise = let (l,r) = splitOn fn xs in (x:l, r)

split :: Eq a => (a -> Bool) -> [a] -> [[a]]
split fn [x]  = [[x | not (fn x)]]
split fn (x:xs)
  | fn x      = [] : split fn xs
  | otherwise =
     let (arr:rest) = split fn xs
     in (x:arr):rest
