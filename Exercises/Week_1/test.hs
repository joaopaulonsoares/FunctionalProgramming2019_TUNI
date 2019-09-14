--calcDistance :: Int -> (Int,Int)

--calcDistance validPoints =
    -- Gera a lista de valores a serem computados
    --  [(x,y) | i <- [0..valueToCompute], y <- [valueToCompute..0] ]
    --          Para gerar lista decrescente [a, a + (signum $ b - a)..b]
    --
    -- Itera na lista avaliando cada valor
    -- calcManhattanDist tCoord rCoord = sum[ (head(tCoord) - head(rCoord)) , (tail(tCoord) - tail(rCoord) ) ]
    -- Caso seja valido, adicione na lista de pontos válidos
    -- retorna lista
    -- Execução encerrada
    manhattanDistance :: [Integer] -> [Integer] -> Integer
    manhattanDistance p q = sum $ zipWith (\ u v -> abs (u-v)) p q


main = do

    --let rightTriangles = [ (),b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]   
    putStrLn "Enter the value of X: "
    input1 <- getLine
    let x = (read input1 :: Int)

    let coordX = [x, x + (signum $ 0 - x)..0]
    let coordY = [0, 0 + (signum $ x - 0)..x]
    let testZ = [(x,y) | x <- [1..5], y <- [6..10] ]

    let testW = [(x,y) | x <- [0..x], y <- [0..x]]
    
    print(coordX)
    print(coordY)
    --print(zip coordX coordY )
    print(testW)
    --print(map coordX coordY)
    
    