import System.Environment (getArgs)
import Data.Array ( Ix(range, inRange), bounds, listArray, Array, array, assocs )
import Data.Sequence (Seq, (|>), empty)
import Data.Array.IArray ((!?), (!), genArray)
import Data.Maybe (mapMaybe)

type Pos = (Int, Int)
type Map = Array Pos TileType
type Queue = Seq Pos
type Graph = Array Pos (Maybe [Pos])

data GameState = GameSt {
                   gsQueue :: Queue,
                   gsGraph :: Graph
                 } deriving (Show)

data TileType = Empty | Roll deriving (Eq, Show)

solve :: String -> Int
solve = length . gsQueue . genGameState . parseData

genGameState :: Map -> GameState
genGameState arr =
  let
    adjacency = genGraph arr
    queue = genQueue adjacency
  in GameSt queue adjacency

genGraph :: Map -> Graph
genGraph arr = genArray (bounds arr) neighboarHay
  where
    neighboarHay :: Pos -> Maybe [Pos]
    neighboarHay pos = case arr ! pos of 
       Empty -> Nothing
       Roll  -> Just . filter justRoll . neighboars $ pos
    justRoll pos = case arr !? pos of
      Just Roll -> True
      _         -> False

genQueue :: Graph -> Queue
genQueue = foldl go empty . assocs
  where
    go :: Queue -> (Pos, Maybe [Pos]) -> Queue
    go queue (_, Nothing) = queue
    go queue (pos, Just n)
      | length n < 4 = queue |> pos
      | otherwise    = queue

parseData :: String -> Map
parseData input =
  let
    rows   = lines input
    height = length rows - 1
    width  = -1 +
      case rows of
        (row:_) -> length row
        []      -> 0 -- 0 length if empty
  in listArray ((0,0), (width, height)) . map go . filter (/= '\n') $ input
  where
    go '.' = Empty
    go '@' = Roll
    go  _  = undefined

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> readFile fname >>= print . solve
    _       -> print "Invalid number of arguments!"

neighboars :: Pos -> [Pos]
neighboars p = [p |+| o | o <- noffset]
  where noffset = [(-1,-1), (0,-1), (1,-1),
                   (-1, 0),         (1, 0),
                   (-1, 1), (0, 1), (1, 1)]

(|+|) :: Pos -> Pos -> Pos
(ax, ay) |+| (bx, by) = (ax + bx, ay + by)


showSym :: Maybe [Pos] -> Char
showSym Nothing  = ' '
showSym (Just p)
  | length p < 4 = 'x'
  | otherwise    = '@'

