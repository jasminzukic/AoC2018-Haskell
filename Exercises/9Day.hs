import           Data.Char               (isNumber)
import           Data.List               (splitAt)
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  (p:[l]) <- map (\x -> read x :: Int) . filter (all isNumber) . words . head . lines <$> readFile "./9DayInput.txt"
  print $ playGameSlow p l          --PART 1
  print $ playGameSlow p (100*l)    --PART 2

playGameSlow :: Int -> Int -> Int
playGameSlow numPlayers largestMarble = go M.empty [0] 0 1
  where
    go scores circle player marble
      -- finish game
      | marble > largestMarble = maximum scores
      -- first 3 marbles
      | marble < 3 = go scores (marble:circle) player' marble'
      -- score counting marble
      | rem marble 23 == 0 =
        case splitAt (length circle - 7) circle of
          (left, picked : right) ->
            go (M.insertWith (+) player (marble + picked) scores)
               (right ++ left)
               player' marble'
      -- normal marble
      | otherwise =
        case splitAt 2 circle of
          (left,right) -> go scores (marble : right ++ left) player' marble'
      where
        player' = rem (player + 1) numPlayers
        marble' = marble + 1
