import System.Environment (getArgs)


type SplitCnt  = Int
type Column    = Int
type Start     = Column
type Beams     = [Column]   -- Invariant -> sorted
type Splitters = [Column]   -- Inviriant -> sorted
type Data      = (Start, [Splitters])

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> readFile fname >>= print . solve

solve :: String -> SplitCnt
solve = fst . emulate . parseData

emulate :: Data -> (SplitCnt, Beams)
emulate (st, rows) = foldl step (0, [st]) rows where
  -- Given splitters and beams on row n -> generate beams on row n+1
  step :: (SplitCnt, Beams) -> Splitters -> (SplitCnt, Beams)
  step (spC, beams) sp = let
      (nc, nb) = go spC beams sp
      fixup (x:y:xs) = if x == y 
        then     fixup (y:xs)
        else x : fixup (y:xs)
      fixup any = any
    in (nc, fixup nb)
  go :: SplitCnt -> Beams -> Splitters -> (SplitCnt, Beams)
  go sc (bi:bs) (si:ss)
    | bi >  si     = mapPair (id, (bi:))               $ go sc     bs      (si:ss) 
    | bi <  si     =                                     go sc     (bi:bs) ss
    | bi == si     = mapPair (id, ((bi+1):).((bi-1):)) $ go (sc+1) bs ss
  go sc beams   _  = (sc, beams)

parseData :: String -> Data
parseData = parseLines . lines where
  parseLines :: [String] -> Data
  parseLines []     = undefined
  parseLines (x:xs) = let
      start     = parseSt 0 x 
      splitters = map parseSp xs
    in (start, splitters)
    where
      parseSt :: Column -> String -> Start
      parseSt n  [] = undefined -- Haven't found start,  programming error => explicit fail
      parseSt n  (x:xs)
        | x == 'S'  = n
        | otherwise = parseSt (n+1) xs
      parseSp :: String -> Splitters
      parseSp = foldl go [] . zip [0..] 
        where go xs (i,s) = if s == '^' then i:xs else xs

mapPair :: (a->c, b->d) -> (a,b) -> (c,d)
mapPair (fna, fnb) (a, b) = (fna a, fnb b)
