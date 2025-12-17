import Prelude hiding (lookup)

import System.Environment (getArgs)
import Data.Map ( Map, empty, insert, lookup, delete, foldlWithKey, assocs )
import qualified Heap as H ( insert )
import Control.Monad.ST (runST, ST)
import Control.Monad (foldM_)
import Data.List (sortBy)
import Data.Ord (comparing, Down (Down))

import Heap ( Heap (Empty), toList )
import UnionFind ( newUnionFind, uniHisto, unite )
import Vec3 ( Vec3, parseV3, calcDist, Dist )

type ID = Int
type DistHeap = Heap (Dist, (ID, ID))

solve :: String -> Int
solve input = let
    (maxID, ptMap) = parsePoints input
    distHeap       = minDistances ptMap
    hist           = runST $ solveDists maxID (map snd . Prelude.take 1000 . toList $ distHeap)
  in product . take 3 . sortBy (comparing Data.Ord.Down) . map snd . assocs $ hist

parsePoints :: String -> (ID, Map ID Vec3)
parsePoints = go 0 empty . lines where
  go :: ID -> Map ID Vec3 -> [String] -> (ID, Map ID Vec3)
  go id cur []     = (id, cur)
  go id cur (x:xs) = let
      nmap = insert id (parseV3 x) cur
      nid  = id + 1
    in go nid nmap xs

minDistances :: Map ID Vec3 -> DistHeap
minDistances = buildHeap 0 Empty where
  buildHeap :: ID -> DistHeap -> Map ID Vec3 -> DistHeap
  buildHeap id heap map = case lookup id map of
    Nothing    -> heap
    (Just vec) -> let
                    nmap  = delete id map
                    nid   = id + 1
                    nheap = foldlWithKey go heap nmap
                  in buildHeap nid nheap nmap
                  where
                    go :: DistHeap -> ID -> Vec3 -> DistHeap
                    go acc rid rvec = H.insert acc (calcDist vec rvec, (id, rid))

solveDists :: ID -> [(ID, ID)] -> ST s (Map ID Int)
solveDists max merges = do
    uf <- newUnionFind max
    foldM_ go uf merges
    uniHisto uf
  where
    go uf (l, r) = do
      unite uf l r
      pure uf

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> readFile fname >>= print . solve

