

manhattanDistance :: Integer -> [(Int,Int)]  -> Bool
manhattanDistance xValue tCoord
    | xValue <= 5 = True
    | xValue > 5 = False 


addVectors :: (Num a) => (a, a) -> (a, a) -> ((a, a))  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)  


--calcManhattanDist :: [Integer] -> [Integer] -> Integer 
--calcManhattanDist tCoord rCoord = sum[ (head(tCoord) - head(rCoord)) , (tail(tCoord) - tail(rCoord) ) ]

manhattanDistance1 :: [Integer] -> [Integer] -> Integer
manhattanDistance1 p q = sum $ zipWith (\ u v -> abs (u-v)) p q

{--
manhattanTest :: Integer -> [(Int,Int)] -> Bool
manhattanTest x rCoord
    | head(rCoord) <= 5 = True
    | x > 5 = False
--}
manhattanTest :: [Integer] -> [Integer] -> Integer
manhattanTest testCoord rootCoord = sum $ zipWith (-) testCoord rootCoord

manhattanTest2 :: Integer -> [Integer] -> [Integer] -> Bool
manhattanTest2 xLimit testCoord rootCoord
    | manhattanCalc < xLimit = True
    | otherwise = False
    where manhattanCalc = sum $ zipWith (-) testCoord rootCoord

main = do 
    
    let rootCordinate = [0,0]
    let testCordinate = [2,3]

    print $ testCordinate
    print $ manhattanTest2 10 testCordinate rootCordinate
    --print $ manhattanDistance 6 [(0,0)]
    --print $ manhattanDistance1 [1,1] [5,5]
    --print $ "oi"
    --print $ addVectors (1,1) (2,2)
    --print $ addVectors rootCordinate testCordinate
    let rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]   