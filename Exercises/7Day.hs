import           Data.Char
import           Data.List.Split            (splitOneOf)
import qualified Data.Map           as M
import           Data.List

-- type Reqs = (String,String)
data WorkQueue = WorkQueue [Char] [(Char,Char)] deriving Show

main :: IO ()
main = do
  xs <- map (toTuple . filter (isUpper)) . lines <$> readFile "./7DayInputT.txt"
  print $ newWorkQueue xs
  print $ part1 xs    --PART 1
  print $ part2 (part1 xs) xs    --PART 2

toTuple (s:a:[b]) = (a,b)

part1 :: [(Char, Char)] -> String
part1 = unfoldr go . newWorkQueue
  where
    go q = do (x, q') <- nextTask q
              return (x, finishTasks [x] q')

newWorkQueue :: [(Char, Char)] -> WorkQueue
newWorkQueue deps = WorkQueue (nub [z | (x,y) <- deps, z <- [x,y]]) deps

nextTask :: WorkQueue -> Maybe (Char, WorkQueue)
nextTask (WorkQueue remaining deps)
  | null candidates = Nothing
  | otherwise       = Just (next, WorkQueue (delete next remaining) deps)
  where
    candidates = remaining \\ map snd deps
    next = minimum candidates

finishTasks :: [Char] -> WorkQueue -> WorkQueue
finishTasks tasks (WorkQueue remaining deps) = WorkQueue remaining deps'
  where
    deps' = filter (\x -> not (fst x `elem` tasks)) deps

part2 js rs = work blankWorkers 0 js [] rs

blankWorkers = (replicate 5 ('_',0))
bw :: [(Char,Int)]
bw = [('a',1),('_',0)]
reqs xs = nub $ map fst xs

-- work :: [(Char,Int)] -> Int -> [Char] -> [Char] -> [Char] -> Int
work ws t [] fs rs
  | and (map ((0>=) . snd) ws) = t

  | otherwise                 = work (map (\(x,y) -> (x,y-1)) ws) (t+1) [] fs rs
work ws t js@(next:jobs) fs rs
  | or (map ((1==) . snd) ws) = work (updateWs ws) (t+1) js fillFs removeRs

  | or (map ((0>=) . snd) ws) && and (map (\j -> notElem j (map snd rs)) js) =
    work (updateWs (fst (newWs ws [] js))) (t+1) (snd (newWs ws [] js)) fs rs

  | otherwise = work (updateWs ws) (t+1) js fs rs


  where
    unlockedJs = filter (\j -> notElem j (map snd rs)) js
    newWs [] nws js = (nws,js)
    newWs ((w,n):ws) nws (j:js)
      | n <= 0 && notElem j (map snd rs) = newWs ws ((w,letterWork j):nws) js
      | otherwise = newWs ws ((w,n):nws) (j:js)
    readyWorkers = (length (filter ((0>=) . snd) ws))
    letterWork c = (ord c - ord 'A') + 1 + 1 -- + 60
    updateWs xs = map (\(x,y) -> (x,y-1)) xs
    finished = map (fst) $ filter ((1==) . snd) ws
    fillFs   = fs ++ finished
    removeRs = filter (\(x,y) -> notElem x finished) rs

-- fillGraph :: [Reqs] -> [String] -> String
-- fillGraph reqs (y:ys)
--   | newY == [] = nub $ concat (y:ys)
--   | otherwise = fillGraph reqs (newY ++ (y:ys))
--   where
--     newY = step reqs y
--
-- step :: [Reqs] -> String -> [String]
-- step xs ys = concatMap (\x -> lookupReq x xs) ys
--
-- lookupReq :: Char -> [Reqs] -> [String]
-- lookupReq x xs = map (sort . snd) $ filter (\(f,s) -> [x] == f) xs
--
-- requirements = M.toList . foldr (\(x,y) m -> M.insertWith (++) y x m) M.empty
--
-- final :: [Reqs] -> String
-- final xs = concat . nub . filter (\[x] -> notElem x (concatMap snd xs)) $ map fst xs

test = [('C','A'),('C','F'),('A','B'),('A','D'),('B','E'),('D','E'),('F','E')]
test2 = [("A","XHOQLUPBYC"),("B","YUFNK"),("C","K"),("D","YK"),("E","TMFBAUILVQ"),("F","SKNX"),("H","PV"),("I","HKCP"),("J","HKYUD"),("L","VCJYQ"),("M","GPJD"),("N","G"),("O","NFISLC"),("P","G"),("Q","FBGKN"),("R","GVC"),("S","GM"),("U","DV"),("W","QLEAHXPKNRS"),("X","IJDKSH"),("Y","V"),("Z","KRBOAWCEQLN")]
test3 = [('G','M'),('T','E'),('P','M'),('V','L'),('Y','B'),('K','Z'),('H','I'),('D','U'),('C','L'),('R','Z'),('U','B'),('J','M'),('M','E'),('I','X'),('N','O'),('S','F'),('X','A'),('F','Q'),('B','Z'),('Q','W'),('L','W'),('O','Z'),('A','Z'),('E','W'),('W','Z'),('G','R'),('H','A'),('A','W'),('Y','D'),('O','A'),('V','U'),('H','W'),('K','F'),('J','X'),('V','R'),('Q','A'),('F','B'),('G','P'),('L','A'),('B','Q'),('H','J'),('J','L'),('F','E'),('U','A'),('G','Q'),('G','S'),('K','J'),('N','B'),('F','O'),('C','Z'),('B','E'),('M','S'),('A','E'),('E','Z'),('K','I'),('P','A'),('Y','L'),('Y','J'),('G','N'),('Q','L'),('D','X'),('C','I'),('K','B'),('N','F'),('D','M'),('B','A'),('U','J'),('Q','Z'),('X','F'),('K','X'),('U','E'),('X','W'),('K','Q'),('I','E'),('D','J'),('P','I'),('K','D'),('S','X'),('C','R'),('P','W'),('I','O'),('S','O'),('K','C'),('N','Q'),('L','E'),('L','Z'),('K','W'),('Y','A'),('L','O'),('N','W'),('R','W'),('C','O'),('H','X'),('V','Y'),('S','W'),('V','E'),('Q','E'),('P','H'),('V','H'),('N','Z'),('C','A')]
