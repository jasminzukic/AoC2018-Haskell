-- {-# LANGUAGE BangPatterns #-}

-- import Control.DeepSeq (force)
import Data.Ord (comparing)
import Data.Foldable (maximumBy)
import qualified Data.Array.Unboxed as A

type Grid = A.Array (Int,Int) Int

main :: IO ()
main = do
  let serial = 4455
  let gridSize = 300
  let cells = mkCells serial gridSize
  print $ part1 cells
  print $ part2 cells

mkCells :: Int -> Int -> Grid
mkCells serial gridSize =
  A.array ((1,1),(gridSize,gridSize))
  [ ((x,y), fuel serial x y) | x <- [1..gridSize], y <- [1..gridSize] ]

fuel :: Int -> Int -> Int -> Int
fuel serial x y = (rackId * y + serial) * rackId `div` 100 `mod` 10 - 5
  where rackId = x + 10

part1 cells = best $ A.assocs $ growSquares cells $ growSquares cells cells

part2 cells = best [ ((x,y,n),e)
                   | (n,vs) <- zip [1..30] (iterate (growSquares cells) cells)
                   , ((x,y),e) <- A.assocs vs ]

best :: Ord b => [(a,b)] -> a
best = fst . maximumBy (comparing snd)

growSquares :: Grid -> Grid -> Grid
growSquares cells prev = -- force
  A.array ((1,1),(n',n'))
  [ ((x,y), prev  A.! (x+1,y+1)
          + cells A.! (x,y)
          + sum [ cells A.! (x,y+z)
                + cells A.! (x+z,y)
                | z <- [1 .. h]
                ]
    )
  | x <- [1..n']
  , y <- [1..n'] ]
  where
    gridSize = fst . snd . A.bounds
    n' = gridSize prev - 1
    h = gridSize cells - gridSize prev + 1
