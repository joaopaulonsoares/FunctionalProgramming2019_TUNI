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

functionTest :: Int -> [(Int,Int)]
functionTest x = [ (a,b) | b <- [0..100], a <- [0..b], ((a - 0) + (b - 0) ) < x]
