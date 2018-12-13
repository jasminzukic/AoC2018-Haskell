data Tree = Tree [Tree] [Int] deriving (Eq, Show)

main :: IO ()
main = do
  xs <- snd . makeNode . map (\x -> read x :: Int) . words <$> readFile "./8DayInput.txt"
  print $ getTreeSum   xs    --PART 1
  print $ getRootValue xs    --PART 2

makeNode :: [Int] -> ([Int], Tree)
makeNode (c1:d1:rest) = (remaining, Tree children metadata)
  where
    remaining = drop d1 restAfter
    metadata  = take d1 restAfter
    (restAfter, children) = makeNodes c1 (rest,[])

makeNodes :: Int -> ([Int], [Tree]) -> ([Int], [Tree])
makeNodes n (xs, trees)
  | n == length trees       = (xs, reverse trees)
  | otherwise               = makeNodes n (nextXs, (nextNode:trees))
  where (nextXs, nextNode) = makeNode xs

getTreeSum :: Tree -> Int
getTreeSum (Tree children ns)
  | length children == 0 = sum ns
  | otherwise            = sum ns + (sum $ map getTreeSum children)

getRootValue :: Tree -> Int
getRootValue (Tree children ns)
  | null children = sum ns
  | otherwise     = sum $ map (\x -> (map getRootValue children) !! (x-1)) newNs
  where newNs = filter (\x -> x <= length children && x > 0) ns

-- part1 :: [Int] -> Int
-- part1 (nodes:metas:ys) = sum $ go [nodes] [metas] ys []
--
-- go :: [Int] -> [Int] -> [Int] -> [Int] -> [Int]
-- go (n:ns) (m:ms) (x:xs) zs
--   | n == 0 && m == 1 && null xs = x : zs                                    -- završi
--   | n == 0 && m == 0            = go ((head ns - 1):(tail ns)) ms (x:xs) zs -- vraćaj se
--   | n == 0 && m >  0            = go (n:ns) (m-1:ms) (xs) (x:zs)            -- novi metadata
--   | otherwise                   = go (x:n:ns) ((head xs):m:ms) (tail xs) zs -- novi node
