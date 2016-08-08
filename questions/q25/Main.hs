
makeString :: [(Int,Int)] -> [((Int,Int), (Int,Int))]
makeString [a] = []
makeString (x:xs) = (x, head xs) : makeString xs


main :: IO ()
main = do
    print $ makeString [(a,b) | a<-[0,1],b<-[0,1,2,3,4,5]]
