import Control.Arrow ((&&&))
import qualified Data.HashMap.Strict as M
import Data.Ix (range)
import Data.List.Split (splitOn)

type Coord = (Int, Int)

dist :: Coord -> Coord -> Int
dist (a, b) (c, d) = abs (a - c) + abs (b - d)

parseCoords :: String -> [Coord]
parseCoords = map (f . splitOn ", ") . lines
    where f [a, b] = (read a, read b)
          f _ = error "Error parsing coord"

allCoordsWithin :: Int -> [Coord] -> [Coord]
allCoordsWithin buffer xs = range ((x0, y0), (x1, y1))
    where x0 = minimum (map fst xs) - buffer
          y0 = minimum (map snd xs) - buffer
          x1 = maximum (map fst xs) + buffer
          y1 = maximum (map snd xs) + buffer

findLargestFiniteArea :: [Coord] -> Int
findLargestFiniteArea xs = maximum $ zipWith f (go 0) (go 10)
    where f a b = if a == b then a else 0
          go n = M.elems $ M.fromListWith (+)
                 [ (snd d, 1) | coord <- allCoordsWithin n xs
                 , let dists = map (dist coord &&& id) xs
                 , let d = minimum dists
                 , length (filter ((== fst d) . fst) dists) == 1
                 ]

part1 :: String -> Int
part1 = findLargestFiniteArea . parseCoords

findRegionWithin :: Int -> [Coord] -> Int
findRegionWithin n xs = length $ filter (\x -> sum (map (dist x) xs) < n)
                        $ allCoordsWithin (n `div` length xs) xs

part2 :: String -> Int
part2 = findRegionWithin 10000 . parseCoords

main = do
  input <- readFile "./6DayInput.txt"
  print $ part1 input
  print $ part2 input

-- import Data.List
-- import Debug.Trace
--
-- manhattan [a, b] [c, d] = ((c, d), abs (a - c) + abs (b - d))
--
-- closestN results point coords = if b /= d then a : results else results
--   where r = map (manhattan point) coords
--         [(a, b), (c, d)] = take 2 $ sortOn snd r
--
-- belowLimit coords point = (< 10000) .  sum $ map (snd . manhattan point) coords
--
-- part1 i j input = map (\i -> (head i, length i)) result
--   where grid = [[x, y] | x <- [0..i], y <- [0..j]]
--         result = group. sort $ foldl' (\acc i -> closestN acc i input) [] grid
--
-- part2 i j input = length $  filter (belowLimit input) grid
--   where grid = [[x, y] | x <- [0..i], y <- [0..j]]
