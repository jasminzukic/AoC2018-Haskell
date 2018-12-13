import Data.List
import           Data.List.Split            (splitOneOf)
import qualified Data.Map             as M

main :: IO ()
main = do
  let gridNum = 4455
  let gridSize = 300
  -- print $ part1 gridNum 3
  print $ bestCellSize gridSize $ M.toList $ part1 gridNum gridSize 300

-- part1 :: Int -> Int -> Int -> M.Map (Int,Int) [Int]
part1 gridNum gridSize 1        = fillGrid (1,1) gridNum 1 gridSize M.empty
part1 gridNum gridSize cellSize = fillGrid (1,1) gridNum cellSize (gridSize - cellSize + 1) $ part1 gridNum gridSize (cellSize-1)
                      -- findMaxCell cellSize $
fillGrid :: (Int,Int) -> Int -> Int -> Int -> M.Map (Int,Int) [Int] -> M.Map (Int,Int) [Int]
fillGrid (x,y) gridNum cellSize maxSize grid
  | x > maxSize = grid
  | y > maxSize = fillGrid (x+1,1) gridNum cellSize maxSize grid
  | otherwise   = fillGrid (x,y+1) gridNum cellSize maxSize $ M.insertWith (++) (x,y) sumFuel grid
  where
    sumFuel = [(if cellSize == 1 then 0 else (last (grid M.! (x,y))))
            + (sum $ map (\(a,b) -> fuel a b) $ [(a,y+cellSize-1) | a <- [x..x+cellSize-2]]
                                             ++ [(x+cellSize-1,b) | b <- [y..y+cellSize-1]])]
    fuel x y = ((((x+10) * y + gridNum) * (x+10)) `div` 100) `mod` 10 - 5

findMaxCell :: Int -> [((Int,Int),[Int])] -> ((Int,Int),(Int,Int))
findMaxCell cellSize xs = ((fst $ head $ filter (\x -> maks == (snd x)!!(cellSize-1)) xs),(maks,cellSize))
  where maks = maximum ((map ((\x -> x !! (cellSize-1)) . snd) xs))

bestCellSize :: Int -> [((Int,Int),[Int])] -> ((Int,Int),Int)
bestCellSize gridSize xs = (fst best, gridSize - (head $ elemIndices maks (snd best)))
  where
    maks = maximum $ concatMap snd xs
    best = head $ filter (\(p,m) -> any (==maks) m) xs



  -- (fst  best, (snd . snd) best)
  -- where
  --   maks = maximum (map (fst . snd) xs)
  --   best = head $ filter (\x -> maks == (fst . snd) x) xs
