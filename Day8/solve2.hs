import Prelude hiding (lookup)

import System.Environment (getArgs)
import Data.Map ( Map, empty, insert, lookup, delete, foldlWithKey, assocs, (!) )
import qualified Heap as H ( insert )
import Control.Monad.ST (runST, ST)
import Control.Monad (foldM_, when)
import Data.List (sortBy)
import Data.Ord (comparing, Down (Down))

import Heap ( Heap (Empty), toList, pop )
import UnionFind ( newUnionFind, uniHisto, unite, find, fullyMerged )
import Vec3 ( Vec3 (xV3), parseV3, calcDist, Dist )

type ID = Int
type DistHeap = Heap (Dist, (ID, ID))

solve :: String -> Int
solve input = let
    (maxID, ptMap) = parsePoints input
    distHeap       = minDistances ptMap
    (ll, lr)       = runST $ connectTilMerged maxID distHeap
    ptl            = ptMap ! ll
    ptr            = ptMap ! lr
  in xV3 ptl * xV3 ptr

connectTilMerged :: ID -> DistHeap -> ST s (ID, ID)
connectTilMerged max_id heap = do
    uf <- newUnionFind max_id
    go uf heap
  where
    go uf heap = case pop heap of
      Nothing                   -> error "heap ran out before merging all points?!"
      Just ((_, (l, r)), nheap) -> do
        connected <- find uf l r
        if connected
          then go uf nheap  -- invariant go is called for not fully merged uf => uf won't be connected with this pair
          else do
            nuf <- unite uf l r
            if fullyMerged nuf
              then pure (l, r)
              else go nuf nheap

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

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fname] -> readFile fname >>= print . solve

