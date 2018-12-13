import           Data.Counter                (count)
import qualified Data.Map             as M   (toList)
import           Data.List                   (foldl', nub)

main :: IO ()
main = do
  xs <- lines <$> readFile "./2DayInput.txt"
  print $ checkSum $ concatMap countAndMultiply xs
  putStrLn $ concat $ nub $ map (findSimilar xs) xs

countAndMultiply :: String -> [Int]
countAndMultiply = nub . filter (\x -> x==2 || x==3) . map snd . M.toList . count

checkSum :: [Int] -> Int
checkSum = product . map snd . M.toList . count

findSimilar :: [String] -> String -> String
findSimilar xss ys = concatMap (differByOne ys) xss

differByOne :: String -> String -> String
differByOne ys xs = if length cross == length xs - 1 then cross else ""
  where cross = map fst $ filter (\(x,y) -> x==y) $ zip xs ys
