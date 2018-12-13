{-# LANGUAGE BangPatterns #-}

import           Data.Char                                (isNumber)
import           Data.Maybe                               (fromJust)
import qualified Data.IntMap.Strict             as IntM
import qualified Data.List.PointedList.Circular as PL

main :: IO ()
main = do
  (p:[l]) <- map (\x -> read x :: Int) . filter (all isNumber) . words <$> readFile "./9DayInput.txt"
  print $ playGameFastest p l          --PART 1
  print $ playGameFastest p (100*l)    --PART 2

playGameFastest :: Int -> Int -> Int
playGameFastest numPlayers largestMarble = go IntM.empty (PL.singleton 0) 0 1
  where
    go !scores !circle !player !marble
      -- finish game
      | marble > largestMarble = maximum $ scores
      -- score counting marble
      | rem marble 23 == 0 = go (IntM.insertWith (+) player (marble + picked) scores)
                                (fromJust (PL.deleteRight circle'))
                                player' marble'
      -- normal marble
      | otherwise = go scores (PL.insertLeft marble (PL.moveN 2 circle)) player' marble'
      where
        circle' = PL.moveN (-7) circle
        picked  = PL._focus circle'
        player' = rem (player + 1) numPlayers
        marble' = marble + 1
