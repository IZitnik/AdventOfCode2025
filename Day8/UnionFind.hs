module UnionFind where 

import Control.Monad ( Monad(return), liftM2, foldM )
import Control.Monad.ST ( ST, runST )
import Data.Array.MArray
    ( newListArray, readArray, writeArray, MArray(newArray), inRange )
import Data.Array.ST ( STUArray, getBounds )
import Data.Map.Lazy ( adjust, empty, insert, lookup, Map, insertWith )
import GHC.Arr ( foldlElems )
import Prelude hiding (lookup)
import Data.Array.Base (freezeSTUArray)
import Data.Array.Unboxed (foldlArray)
 

data UnionFind s = UnionFind {
    grps :: Int
  , ids  :: STUArray s Int Int
  , szs  :: STUArray s Int Int
  }


newUnionFind :: Int -> ST s (UnionFind s)
newUnionFind n = liftM2 (UnionFind (n-1)) (newListArray (0, n-1) [0..n-1]) (newArray (0, n-1) 1)

fullyMerged :: UnionFind s -> Bool
fullyMerged uf = grps uf == 0

find :: UnionFind s -> Int -> Int -> ST s Bool
find uf p q = liftM2 (==) (root uf p) (root uf q)

root :: UnionFind s -> Int -> ST s Int
root uf i = do
  id <- readArray (ids uf) i
  if id /= i
    then do
      gpid <- readArray (ids uf) id
      writeArray (ids uf) i gpid
      root uf id
    else return i

unite :: UnionFind s -> Int -> Int -> ST s (UnionFind s)
unite uf p q = do
  i <- root uf p
  j <- root uf q
  szi <- readArray (szs uf) i
  szj <- readArray (szs uf) j
  if szi < szj
    then do
      writeArray (ids uf) i j
      writeArray (szs uf) j (szi + szj)
    else do
      writeArray (ids uf) j i
      writeArray (szs uf) i (szj + szi)
  pure $ if i == j
    then uf
    else _mergedGrps uf

uniHisto :: UnionFind s -> ST s (Map Int Int)
uniHisto uf = do
    (lo, hi) <- getBounds (ids uf)
    let indices = [lo .. hi]
    foldM go empty indices
  where
    go m i = do
      r <- root uf i
      return $ insertWith (+) r 1 m 

-- isFullyMerged :: UnionFind s -> ST s Bool
-- isFullyMerged uf = do
--

main = runST $ do 
  uf <- newUnionFind 4
  uf <- unite uf 1 2
  uf <- unite uf 0 1
  uf <- unite uf 0 1
  uf <- unite uf 3 1
  pure $ fullyMerged uf

_mergedGrps :: UnionFind s -> UnionFind s
_mergedGrps (UnionFind grps ids szs) = UnionFind (grps-1) ids szs
