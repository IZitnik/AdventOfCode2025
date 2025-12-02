import System.Environment (getArgs)

step :: (Int, Int) -> Int -> (Int, Int)
step (state, cur) offset = 
 let value = mod (cur + offset) 100
 in case value of
  0 -> (state+1, value)
  _ -> (state  , value)

parseLine :: String -> Int
parseLine ('L':num) = -read num :: Int
parseLine ('R':num) =  read num :: Int

parseContent :: String -> Int
parseContent = fst . foldl step (0, 50) . map parseLine . lines

main = do  
  args <- getArgs
  case args of
    []      -> print "Expcted filename as argument"
    fname:_ -> readFile fname >>= print . parseContent
