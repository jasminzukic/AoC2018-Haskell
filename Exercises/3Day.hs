import           Data.List.Split            (splitOneOf)
import qualified Data.Map           as M
import           Data.List                  (isInfixOf, nub)

main :: IO ()
main = do
  xs <- map (map (\x -> read x :: Int) . filter (/= "") . splitOneOf "# @,:x") <$> lines <$> readFile "./3DayInput.txt"
  let field = M.unionsWith (\x y -> x ++(',': y)) $ map fillField' xs
  print $ M.size $ M.filter (any (==',')) field                                             --PART 1
  putStrLn $ head $ filter (isAlone' field) $ nub $ M.elems $ M.filter (all (/=',')) field  --PART 2

fillField' :: [Int] -> M.Map (Int,Int) String
fillField' (idt:x:y:h:[w]) = M.fromList [((a,b),show idt) | a <- [x+1..x+h], b <- [y+1..y+w]]

isAlone' :: M.Map (Int,Int) String -> String -> Bool
isAlone' m i = M.null $ M.filter (any (==',')) $ M.filter (isInfixOf i) m
