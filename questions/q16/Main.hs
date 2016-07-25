import Data.List

solveCount :: Int -> Int
solveCount n = length $ solve n

solve :: Int -> [(Int, Int, Int)]
solve n = let c = n `div` 4
              d = c * c
          in nub [(a,b,d) | even n, a <- makeRect n, b <- makeRect n, a + b == d]


makeRect :: Int -> [Int]
makeRect a
  | odd a = []
  | otherwise =  map (\(x,y) -> x*y) $ zip [1..a `div` 2 - 1] $ reverse [1..a `div` 2 - 1]

main :: IO ()
main = do
  print $ sum $ map solveCount [1..500]
