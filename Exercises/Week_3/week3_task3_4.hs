{--
Write a function that is given:
- f, a distance function of type String -> String -> Float (like the ones in Task 3.2)
- d :: Float
- ss :: [String]
For each string in ss it computes a list of similar strings in ss (strings that are at most distance d from the string).

Calling this function with fuction of
3.2a), d=0.3 and ss=["aaabc", "aabdd", "a", "aa", "abdd", "bcbcb", "", "abcdefghij"] 
    should return [["aaabc","aabdd","aa","bcbcb"],["aaabc","aabdd","abdd"],["a","aa"],["aaabc","a","aa"],["aabdd","abdd"],["aaabc","bcbcb"],[""],["abcdefghij"]].
3.2b), d=0.2 and ss=["123a","456789b","45","abc", "ab1", "a12", "abcdefghij"] 
    should return [["456789b","45"],["123a","456789b","45","a12"],["123a","456789b","45","a12"],[],[],["456789b","45"],[]].

TEST CASES
    -> function functionMeasureDistanceA 0.3 "aaabc" ["aabdd","a","aa","abdd","bcbcb","", "abcdefghij"]    
    -> function functionMeasureDistanceB 0.2 "123a" ["456789b","45","abc", "ab1", "a12", "abcdefghij"]
--}
function :: (String -> String -> Float) -> Float -> [String] -> [[String]]
function f d (x:xs)
    | xs == [] = [[]]
    | otherwise = adaptorFunction f d x (x:xs) (x:xs)

adaptorFunction :: (String -> String -> Float) -> Float -> String -> [String] -> [String] -> [[String]]
adaptorFunction _ _ _ [] _ = []
adaptorFunction f d z (s:ss) list = (functionDistance f d s list):(adaptorFunction f d z ss list)
    where functionDistance f d z ss = [ s | s <- ss, (f z s) <= d]  

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