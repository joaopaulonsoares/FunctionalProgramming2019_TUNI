{--
Write a function that is given:
- f, a distance function of type String -> String -> Float (like the ones in Task 3.2)
- d :: Float
- z :: String
- ss : [String]
Your function makes a list of strings w in ss such that the distance between w and z is at most d (as calculated with f, given as a parameter to your 
function).

Do this by
(a) basic recursion
(b) list comprehension
(c) foldl
(d) using the filter function

Calling these functions with fuction of
3.2a), d=0.3, z="aaabc" and ss=["aabdd","a","aa","abdd","bcbcb","", "abcdefghij"] should return ["aabdd","aa","bcbcb"].
3.2b), d=0.2 ,z="123a" and ss=["456789b","45","abc", "ab1", "a12", "abcdefghij"] should return ["456789b","45"].

TEST CASES

    => (A)
        -> functionA functionMeasureDistanceB 0.2 "123a" ["456789b","45","abc", "ab1", "a12", "abcdefghij"]
        -> functionA functionMeasureDistanceA 0.3 "aaabc" ["aabdd","a","aa","abdd","bcbcb","", "abcdefghij"]
    => (B)
        -> functionB functionMeasureDistanceA 0.3 "aaabc" ["aabdd","a","aa","abdd","bcbcb","", "abcdefghij"]
        -> functionB functionMeasureDistanceB 0.2 "123a" ["456789b","45","abc", "ab1", "a12", "abcdefghij"]
    
    => (C)
        -> functionC functionMeasureDistanceA 0.3 "aaabc" ["aabdd","a","aa","abdd","bcbcb","", "abcdefghij"]
        -> functionC functionMeasureDistanceB 0.2 "123a" ["456789b","45","abc", "ab1", "a12", "abcdefghij"]
    
    => (D)
        -> functionD functionMeasureDistanceA 0.3 "aaabc" ["aabdd","a","aa","abdd","bcbcb","", "abcdefghij"]
        -> functionD functionMeasureDistanceB 0.2 "123a" ["456789b","45","abc", "ab1", "a12", "abcdefghij"]
--}

-- (A) Basic Recursion
functionA :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
functionA _ _ _ [] = []
functionA f d z (s:ss)
    | (f z s) <= d = s:(functionA f d z ss)
    | otherwise = functionA f d z ss

-- (B) List Comprehension
functionB :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
functionB f d z ss = [ s | s <- ss, (f z s) <= d]  

-- (C) Foldl
functionC :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
functionC f d z ss =  foldl (\def x -> if (f z x) <= d then x : def else def) [] ss

-- (D) Using the filter function
functionD :: (String -> String -> Float) -> Float -> String -> [String] -> [String]
functionD f d z ss = filter (\s -> (f z s) <= d) ss

-- =================================================  Auxiliar Functions ===========================================
functionMeasureDistanceA :: String -> String -> Float
functionMeasureDistanceA [] [] = 0
functionMeasureDistanceA x y = (countNotXinY + countNotYinX) / (sizeX + sizeY)
    where countNotXinY = countCharsNotInString x y
          countNotYinX = countCharsNotInString y x
          sizeX = fromIntegral(length x)
          sizeY = fromIntegral(length y)

countCharsNotInString :: String -> String -> Float
countCharsNotInString _ [] = 0
countCharsNotInString [] _ = 0
countCharsNotInString (x:xs) ys
    | ys == [] = fromIntegral(length (x:xs))
    | (x `elem` ys) == True  = countCharsNotInString xs ys
    | otherwise = 1 + (countCharsNotInString xs ys )



functionMeasureDistanceB :: String -> String -> Float
functionMeasureDistanceB [] [] = 0
functionMeasureDistanceB x y = (countNotCharsInX + countNotCharsInY) / (sizeX + sizeY)
    where countNotCharsInX = countCharsNotDigits x
          countNotCharsInY = countCharsNotDigits y
          sizeX = fromIntegral(length x)
          sizeY = fromIntegral(length y)

digits = ['0'..'9']
countCharsNotDigits :: String  -> Float
countCharsNotDigits [] = 0
countCharsNotDigits (x:xs)
    | (x `elem` digits) == True  = countCharsNotDigits xs
    | otherwise = 1 + (countCharsNotDigits xs)