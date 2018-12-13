import Data.Foldable (foldl')
import Data.Array.Unboxed (array, (!), UArray, bounds)
import Data.Array.ST (STUArray, thaw, runSTUArray, writeArray, readArray)
import Control.Monad.ST (ST)
import Data.Maybe (catMaybes)

power :: Int -> (Int, Int) -> Int
power serial (x, y) = let rackID = x + 10
  in ((rackID * y + serial) * rackID) `div` 100 `mod` 10 - 5

part1 :: Int -> (Int, Int)
part1 serial = let cells = [(x, y)| x <- [1..298], y <- [1..298]]
  in snd $ foldl' compute (0, (0, 0)) cells
  where
    compute cur candidate = max cur (gridSum candidate, candidate)
    gridSum (x, y)        =
      sum [power serial (x', y')| x' <- [x..x+2], y' <- [y..y+2]]

powerGrid :: Int -> Word -> Word -> UArray (Word, Word) Int
powerGrid serial xMax yMax = array ((1, 1), (xMax, yMax)) mkAssocs
  where
    compute (x, y) = power serial (fromIntegral x, fromIntegral y)
    mkAssocs = [((x, y), compute (x, y)) | x <- [1..xMax], y <- [1..yMax]]

newtype SAT = SAT {get :: UArray (Word, Word) Int}

summedAreaTable :: UArray (Word, Word) Int -> SAT
summedAreaTable matrix = let
  ((rMin, cMin), (rMax, cMax)) = bounds matrix
  compute :: STUArray s (Word, Word) Int -> Word -> Word -> ST s ()
  compute m r c = do
    l  <- if c == cMin              then pure 0 else readArray m (r    , c - 1)
    u  <- if r == rMin              then pure 0 else readArray m (r - 1, c    )
    lu <- if c == cMin || r == rMin then pure 0 else readArray m (r - 1, c - 1)
    writeArray m (r, c) $ matrix ! (r, c) + l + u - lu
  in SAT $ runSTUArray $ do
    mut <- thaw matrix
    sequence_ [compute mut r c | r <- [rMin..rMax], c <- [cMin..cMax]]
    pure mut

sumOver :: SAT -> (Word, Word) -> Word -> Maybe Int
sumOver (SAT table) (x, y) 1    = Just $ table ! (x, y)
sumOver (SAT table) (x, y) edge = let
  ((rMin, cMin), (rMax, cMax)) = bounds table
  in if rMin <= x && rMax >= x + edge - 1 && cMin <= y && cMax >= y + edge - 1
  then Just $
       table ! (x + edge - 1, y + edge - 1)
       + (if x == rMin || y == cMin then 0 else table ! (x - 1, y - 1))
       - (if x == rMin then 0 else table ! (x - 1, y + edge - 1))
       - (if y == cMin then 0 else table ! (x + edge - 1, y - 1))
  else Nothing

part2 :: SAT -> (Int, (Word, Word, Word))
part2 sat@(SAT table) = let
  ((rMin, cMin), (rMax, cMax)) = bounds table
  maxEdgeLen                   = min (rMax - rMin) (cMax - cMin) + 1
  f (a, b)                     = a >>= \x -> pure (x, b)
  in maximum . catMaybes $
      [f (sumOver sat (x, y) edge, (x, y, edge))
        | edge <- [1..maxEdgeLen],
          x <- [rMin..rMax - edge + 1],
          y <- [cMin..cMax - edge + 1]]

main :: IO ()
main = do
  let input = 4455
      sat   = summedAreaTable $ powerGrid input 300 300
  print $ part1 input
  print $ part2 sat
