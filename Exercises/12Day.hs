import Data.List (elemIndices)

main :: IO ()
main = do
  ini <- drop 15 . head . lines <$> readFile "./12DayInput.txt"
  let startNum = 10
  let endNum = 150
  let initial = replicate startNum '.' ++ ini ++ replicate (endNum+5) '.'
  print $ part1 startNum 20     initial
  print $ part2 startNum endNum initial

part1 startNum endNum initial = sum $ map (\x -> x-startNum) $ elemIndices '#' lastGen
  where lastGen = (iterate (\x -> ".." ++ nextGen x) initial) !! (endNum)

part2 s e i = part1 s e i + (50000000000-e)*(part1 s (e+1) i - part1 s e i)

nextGen :: String -> String
nextGen xs@(a:b:c:d:[e]) = next xs : ".."
nextGen xs               = next (take 5 xs) : nextGen (tail xs)

next :: String -> Char
next s
  | s == "#...#" = '#'
  | s == "##..#" = '#'
  | s == ".#.##" = '#'
  | s == "###.#" = '#'
  | s == ".#.#." = '#'
  | s == "#.##." = '#'
  | s == "..#.#" = '#'
  | s == ".#..." = '#'
  | s == ".##.." = '#'
  | s == ".#..#" = '#'
  | s == "...##" = '#'
  | s == "#.#.#" = '#'
  | s == "####." = '#'
  | s == "###.." = '#'
  | s == "##..." = '#'
  | otherwise    = '.'
