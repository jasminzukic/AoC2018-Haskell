import           Data.List.Split            (splitOn)
import qualified Data.Map           as M
import           Data.Ix                    (range)
import Control.Arrow ((&&&))
import           Data.List                  (foldl', isInfixOf, nub, sort)

type XMin = Int
type XMax = Int
type YMin = Int
type YMax = Int

type Coord = (Int, Int)

main :: IO ()
main = do
  xs <- map (toTuple . map (\x -> read x :: Int) . splitOn ",") <$> lines <$> readFile "./6DayInputT.txt"
  print $ part1 xs   --PART 1
  -- print $ part2    --PART 2

toTuple (x:[y]) = (x,y)

part1 xs = (grid 0 xs)
-- part1 xs = maximum $ zipWith same (grid 0 xs) (grid 5 xs)

same a b = if a == b then a else 0
grid n xs = findGrid n xs
dists n xs = map (dist (grid n xs) &&& id) xs

-- findLargestFiniteArea :: [Coord] -> Int
-- findLargestFiniteArea xs = maximum $ zipWith f (go 0) (go 10)
--     where f a b = if a == b then a else 0
--           go n = M.elems $ M.fromListWith (+)
--                  [ (snd d, 1) | coord <- allCoordsWithin n xs
--                  , let dists = map (dist coord &&& id) xs
--                  , let d = minimum dists
--                  , length (filter ((== fst d) . fst) dists) == 1
--                  ]

dist :: Coord -> Coord -> Int
dist (a, b) (c, d) = abs (a - c) + abs (b - d)

findGrid :: Int -> [Coord] -> [(Int,Int)]
findGrid n xs = range $ foldr (step n) (head xs, head xs) xs
  where
    step n (a,b) ((x1,y1),(x2,y2)) = (((min a x1) - n, min b y1 - n),(max a x2 + n, max b y2 + n))

-- fillGrid :: [Coord] -> M.Map Coord (String, Int)
fillGrid xs grid@((x1,y1),(x2,y2)) = undefined

  -- foldr (fillGrid' xs 1) M.empty grid
  -- where
    -- fillGrid' :: (Int,Int) -> M.Map (Int, Int) (String, Int) -> M.Map (Int,Int) (String, Int)
    -- fillGrid' grid@((xMin, yMin), (xMax, yMax)) (a,b) m =
