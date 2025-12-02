import System.Environment (getArgs)

step :: (Int, Int) -> Int -> (Int, Int)
step (state, cur) offset = 
 let clickCnt new
       | new == 0  = 1
       | new < 0   = (if cur == 0 then 0 else 1) + (new `div` (-100))
       | otherwise = new `div` 100
 in (clickCnt (cur + offset) + state, mod (cur + offset) 100)

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
