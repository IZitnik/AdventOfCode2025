module Heap where

data Heap a = Empty
            | Node a (Heap a) (Heap a) deriving Show

singleton :: Ord a => a -> Heap a
singleton x = Node x Empty Empty

empty :: Heap a
empty = Empty

merge :: Ord a => Heap a -> Heap a -> Heap a
merge Empty t2 = t2
merge t1 Empty = t1
merge t1@(Node x1 l1 r1) t2@(Node x2 l2 r2)
    | x1 <= x2  =   let r = (t2 `merge` r1) in Node x1 r l1
    | otherwise =   let r = (t1 `merge` r2) in Node x2 r l2

insert :: Ord a => Heap a -> a -> Heap a
insert heap x = singleton x `merge` heap

peak :: Ord a => Heap a -> Maybe a
peak (Node x _ _) = Just x
peak Empty = Nothing

deleteMin :: Ord a => Heap a -> Maybe (Heap a)
deleteMin (Node _ l r) = Just $ merge l r
deleteMin Empty = Nothing

pop :: Ord a => Heap a -> Maybe (a, Heap a)
pop heap = do
    min <- peak heap
    rest <- deleteMin heap
    return (min, rest)

fromList :: Ord a => [a] -> Heap a
fromList = foldl insert Empty

toList :: Ord a => Heap a -> [a]
toList heap  = case pop heap of 
  Just (el, rest) -> el : toList rest
  Nothing         -> []

heapSort :: Ord a => [a] -> [a]
heapSort = toList . fromList
