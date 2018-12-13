import           Data.List.Split            (splitOneOf)

main :: IO ()
main = do
  xs <- map (getNum . splitOneOf "<,><,>") . lines <$> readFile "./10DayInput.txt"
  let (message, time) = part1 xs 0
  putStr message
  print time

getNum :: [String] -> [Int]
getNum (_:x:y:_:vx:vy:_) = map read $ x:y:vx:[vy]

part1 :: [[Int]] -> Int -> (String, Int)
part1 points t
  | wordsClose = (printGrid $ putToZero points,t)
  | otherwise  = part1 (map updatePoint points) (t+1)
  where
    wordsClose                  = all hasNeighbour points
    updatePoint (x:y:vx:[vy])   = (x+vx):(y+vy):vx:[vy]
    hasNeighbour xs             = any (isNeighbour xs) points
    isNeighbour (x:y:_) (a:b:_) = (x/=a || y/=b) && 2 >= (abs (a-x) + abs (b-y))

putToZero :: [[Int]] -> [[Int]]
putToZero ys = map (normalizeP (minimum (map head ys)) (minimum (map (!!1) ys))) ys
  where normalizeP a b (x:y:ys) = (x-a):(y-b):ys

printGrid :: [[Int]] -> String
printGrid ys = toString (coordYs) (0,0) (maximum $ map fst coordYs) (maximum $ map snd coordYs)
  where
    coordYs = map (\(x:y:_) -> (y,x)) ys

toString :: [(Int,Int)] -> (Int,Int) -> Int -> Int -> String
toString ys (tempX,tempY) maxX maxY
  | tempX >= maxX && tempY > maxY = "\n"
  | tempY > maxY            = '\n':toString ys (tempX+1,0)       maxX maxY
  | (tempX,tempY) `elem` ys = '#' :toString ys (tempX  ,tempY+1) maxX maxY
  | otherwise               = '.' :toString ys (tempX  ,tempY+1) maxX maxY
