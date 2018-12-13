import Data.Char (toUpper)

main :: IO ()
main = do
  xs <- part1 . head . lines <$> readFile "./5DayInput.txt"
  print $ length xs
  print $ part2  xs

part1 :: String -> String
part1 = foldr collapse ""
  where collapse x (y:ys) | x/=y && toUpper x == toUpper y = ys
        collapse x ys = x:ys

part2 :: String -> Int
part2 ps = minimum [length $ part1 (filter (isDiff c) ps) | c <- ['a'..'z']]
  where isDiff c x = x/=toUpper c && x/=c
