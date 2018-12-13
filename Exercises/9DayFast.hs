import           Data.Char                  (isNumber)
import qualified Data.IntMap.Strict as IntM
import qualified Data.Sequence      as Seq

main :: IO ()
main = do
  (p:[l]) <- map (\x -> read x :: Int) . filter (all isNumber) . words <$> readFile "./9DayInput.txt"
  print $ playGameFast p l          --PART 1
  print $ playGameFast p (100*l)    --PART 2

playGameFast :: Int -> Int -> Int
playGameFast numPlayers largestMarble = go IntM.empty (Seq.singleton 0) 0 1
  where
    go scores circle player marble
      -- finish game
      | marble > largestMarble = maximum $ scores
      -- first 3 marbles
      | marble < 3 = go scores (marble Seq.<| circle) player' marble'
      -- score counting marble
      | rem marble 23 == 0 =
        case Seq.splitAt (Seq.length circle - 7) circle of
          (left, picked Seq.:<| right) ->
            go (IntM.insertWith (+) player (marble + picked) scores)
               (right Seq.>< left)
               player' marble'
      -- normal marble
      | otherwise =
        case Seq.splitAt 2 circle of
          (left,right) -> go scores (marble Seq.<| right Seq.>< left) player' marble'
      where
        player' = rem (player + 1) numPlayers
        marble' = marble + 1
