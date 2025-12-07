import System.Environment (getArgs)

type Column    = Int
type Start     = Column
type Count     = Int
type Beams     = [(Column, Count)]   -- Invariant -> sorted
type Splitters = [Column]   -- Inviriant -> sorted
type Data      = (Start, [Splitters])


main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> readFile fname >>= print . solve

solve :: String -> Int
solve = sum . map snd . emulate . parseData

emulate :: Data -> Beams
emulate (st, rows) = foldl step [(st, 1)] rows where
  -- Given splitters and beams on row n -> generate beams on row n+1
  step :: Beams -> Splitters -> Beams
  step beams sp = let
      nb = go beams sp
      fixup ((ai,av):(bi,bv):xs) = if ai == bi 
        then           fixup ((ai, av+bv):xs)
        else (ai,av) : fixup ((bi, bv)   :xs)
      fixup any = any
    in fixup nb
  go :: Beams -> Splitters -> Beams
  go ((bc,bv):bs) (si:ss)
    | bc >  si     = (bc,bv)                : go bs           (si:ss) 
    | bc <  si     =                          go ((bc,bv):bs) ss
    | bc == si     = (bc+1,bv) : (bc-1, bv) : go bs           ss
  go beams   _     = beams

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
