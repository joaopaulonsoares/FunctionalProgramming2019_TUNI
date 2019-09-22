distFilt :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
distFilt f d z ss = [w | w <- ss, f w z <= d]

simStrings :: (String -> String -> Float) -> Float -> [String] -> [[String]]
simStrings f d ss = fxoldr (\z acc -> (distFilt f d z ss):acc) [] ss


--Function from Task 3.3. Function that uses list comprehension was chosen, but you could have chosen any of them, although using any of them was not required.
listComp :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
listComp f d z ss = [x | x <- ss, f x z <= d] 

answer :: (String -> String -> Float) -> Float -> [String] -> [[String]]
answer f d ss = map theFunction ss
    where theFunction s = listComp f d s ss