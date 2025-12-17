module Vec3 where

type Dist = Int
data Vec3 = V3 { xV3 :: Int, yV3 :: Int, zV3 :: Int }
  deriving (Show, Eq, Ord)

parseV3 :: String -> Vec3
parseV3 str = case splitBy (== ',') str of
  [x, y, z] -> V3 (read x) (read y) (read z)
  _         -> error "Invalid Vec3 format"

calcDist :: Vec3 -> Vec3 -> Dist
calcDist (V3 ax ay az) (V3 bx by bz) = (ax-bx)^2 + (ay-by)^2 + (az-bz)^2

splitBy :: Eq a => (a -> Bool) -> [a] -> [[a]]
splitBy p str = case dropWhile p str of
  [] -> []
  s' -> w : splitBy p s''
      where (w, s'') = break p s'
