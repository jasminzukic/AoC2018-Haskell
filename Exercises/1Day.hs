main :: IO ()
main = do
  s <- readFile "./1DayInput.txt"
  print $ sum $ map removePlus $ lines s
  print $ findTwice [] 0 $ cycle $ map removePlus $ lines s

removePlus :: String -> Int
removePlus s = if head s == '+' then read (tail s) else read s

findTwice :: [Int] -> Int -> [Int] -> Int
findTwice fs f xs
  | f `elem` fs = f
  | otherwise   = findTwice (f:fs) (f + head xs) $ tail xs
