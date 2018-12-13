import           Data.List.Split            (splitOneOf)
import qualified Data.Map           as M
import           Data.List                  (foldl', isInfixOf, nub, sort)

main :: IO ()
main = do
  xs <- sort <$> map (filter (/="") . splitOneOf "[-- :]#") <$> lines <$> readFile "./4DayInputTest.txt"
  let timeTable  = expand $ transformList 0 [] xs
  let countSleep = foldl' (\acc ((i,m),s) -> M.insertWith (+) (i,m) s acc) M.empty timeTable
  print $ part1 countSleep    --PART 1
  print $ part2 countSleep    --PART 2

part1 :: M.Map (Int,Int) Int -> Int
part1 xs = (findMax' ((0,0),0) $ M.toList $ M.filterWithKey (\k _ -> fst k == sleeperID) xs)
  where
    sleeperID = findMax (0,0) $ M.toList $ foldl' (\acc ((i,m),s) -> M.insertWith (+) i s acc) M.empty $ M.toList xs

part2 = findMax' ((0,0),0) . M.toList

findMax' :: ((Int,Int),Int) -> [((Int,Int),Int)] -> Int
findMax' ((i,n),_) [] = i * n
findMax' frst@((i,n),s) (scnd@((i2,n2),s2):rest)
  | s2 > s    = findMax' scnd rest
  | otherwise = findMax' frst rest

findMax :: (Int,Int) -> [(Int,Int)] -> Int
findMax (i,n) [] = i
findMax (i,n) ((i2,n2):rest)
  | n2 > n    = findMax (i2,n2) rest
  | otherwise = findMax (i,n) rest

transformList :: Int -> [((Int,Int),Int)] -> [[String]] -> [((Int,Int),Int)]
transformList i l [] = reverse l
transformList i l ((y:m:d:h:mn:a:b:c):rest)
  | a == "Guard"  = transformList (r b) (((r b,  0),0):l) rest
  | a == "falls"  = transformList i     (((i, r mn),1):l) rest
  | otherwise     = transformList i     (((i, r mn),0):l) rest
  where r x = read x :: Int

expand :: [((Int,Int), Int)] -> [((Int,Int), Int)]
expand [a] = [a]
expand (frst@((i,m),s):scnd@((_,m2),s2):rest)
  | m == 59 || m + 1 == m2 && s /= s2  = frst:(expand (scnd:rest))
  | otherwise                          = frst:(expand (((i,m+1),s):scnd:rest))
