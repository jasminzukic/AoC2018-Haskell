import Data.List
import qualified Data.IntMap          as IntM
import           Data.List.Split              (splitOneOf)

-- data Item = Nill | Track | Wagon
-- data Track = Track (Int,Int) [(Int,Int)]
data Orientation = W | N | E | S deriving (Eq,Ord,Show)
data Turn = L | F | R deriving (Eq,Ord,Show)
data Wagon = Wagon Position Orientation Turn deriving (Eq,Ord,Show)

type Position = (Int,Int)

main :: IO ()
main = do
  xs <- lines <$> readFile "./13DayInput.txt"
  let h = length xs
  let wagons = map wagonize $ findWagons xs h
  let cleanGrid = map (map clearGrid) xs
  print cleanGrid
  print wagons
  print $ part1 wagons cleanGrid
  print $ part2 wagons cleanGrid

findWagons :: [String] -> Int -> [((Position),Char)]
findWagons xs h = filter (\(p,c) -> c=='^' || c=='>' || c=='v' || c=='<')
                           [ ((x,y),(xs!!x)!!y) | x <- [0..(h-1)], y <- [0..(length (xs!!x) - 1)]]

wagonize :: ((Position),Char) -> Wagon
wagonize ((x,y),c) =
  case c of
    '^' -> Wagon (x,y) N L
    '>' -> Wagon (x,y) E L
    'v' -> Wagon (x,y) S L
    '<' -> Wagon (x,y) W L
     -- _  -> error "buraaaaz, ne valja ti vagon!"

clearGrid :: Char -> Char
clearGrid c
  | c == '^' || c == 'v' = '|'
  | c == '<' || c == '>' = '-'
  | otherwise = c

-- part1 :: [Wagon] -> [String]
part1 wagons grid
  | isCrash = findCrash
  | otherwise = part1 newWagons grid
  where
    newWagons = map (step grid) wagons
    positions = map (\(Wagon p _ _) -> p) wagons
    isCrash   = nub positions /= positions
    findCrash = head $ head $ filter (\x -> length x > 1) $ group $ sort positions

part2 wagons grid
  | length newWagons == 1 = newWagons
  -- | isCrash   = part2 (sort removeCrashed) grid
  | otherwise = part2 (sort newWagons) grid
  where
    newWagons = updateWagons grid wagons []
    positions = map (\(Wagon p _ _) -> p) newWagons
    isCrash   = nub positions /= positions
    findCrash = nub $ concat $ filter (\x -> length x > 1) $ group $ sort positions
    removeCrashed = filter (\(Wagon p _ _) -> p `notElem` findCrash) newWagons

updateWagons :: [String] -> [Wagon] -> [Wagon] -> [Wagon]
updateWagons grid [] ys = ys
updateWagons grid (w:ws) ys
  | (pos newWagon) `elem` positions ws
  || (pos newWagon) `elem` positions ys =
    updateWagons grid (updateWs (pos newWagon) ws) (updateWs (pos newWagon) ys)

  | otherwise = updateWagons grid ws (newWagon:ys)
  where
    newWagon = step grid w
    pos (Wagon p _ _) = p
    positions xs = map (\(Wagon p _ _) -> p) xs
    updateWs w ws = filter (\(Wagon p _ _) -> p /= w) ws


step :: [String] -> Wagon -> Wagon
step grid (Wagon p o t)
  | next p o == '+' = Wagon (nextPosition p o) (nextOrientation o t) (nextTurn t)
  | next p o == '|' || next p o == '-' = Wagon (nextPosition p o) o t
  | next p o == '\\' = Wagon (nextPosition p o) (backSlash o) t
  | next p o == '/' = Wagon (nextPosition p o) (slash o) t
  where
    next (x,y) N = (grid!!(x-1))!! y
    next (x,y) E = (grid!!x    )!!(y+1)
    next (x,y) S = (grid!!(x+1))!! y
    next (x,y) W = (grid!!x    )!!(y-1)

backSlash E = S
backSlash N = W
backSlash W = N
backSlash S = E

slash E = N
slash N = E
slash W = S
slash S = W

nextPosition :: Position -> Orientation -> Position
nextPosition (x,y) N = (x-1,y  )
nextPosition (x,y) E = (x  ,y+1)
nextPosition (x,y) S = (x+1,y  )
nextPosition (x,y) W = (x  ,y-1)

nextOrientation :: Orientation -> Turn -> Orientation
nextOrientation N turn
  | turn == L = W
  | turn == F = N
  | turn == R = E
nextOrientation E turn
  | turn == L = N
  | turn == F = E
  | turn == R = S
nextOrientation S turn
  | turn == L = E
  | turn == F = S
  | turn == R = W
nextOrientation W turn
  | turn == L = S
  | turn == F = W
  | turn == R = N

nextTurn :: Turn -> Turn
nextTurn L = F
nextTurn F = R
nextTurn R = L
