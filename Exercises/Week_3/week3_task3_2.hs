{--
Write the following functions that measure distance between two strings x and y. Please note that they are not standard well-behaving distance functions.
Notice that e.g. length of a list is an Int and you can get a fractional (non-integer) value out of that with the fromIntegral function.
The type of both functions should be: String -> String -> Float

a)
( (count of how many of the characters in x do not appear in y) + (count of how many of the characters in y do not appear in x)) / ((length of x) + length of y))
If both lists are empty, then the distance is 0.
For example, the distance between "aaabc" and "aabdd" with this function is (1 + 2) / (5 + 5).
In "aaabc" 'a' and 'b' appear in "aabdd" but 'c' does not so we get 1; in "aabdd" 'a' and 'b' appear in "aaabc" but 'd' does not so we get 2; both "aaabc" and "aabdd" are of length 5.

b)
(count of characters in x that are other than any of '0'..'9') + (count of characters in y that are other than any of '0'..'9') / ((length of x) + (length of y))
If both lists are empty, then the distance is 0.
For example for "xy765" and "abc2311" the result is (2+3)/(5+7)
--}

functionOne :: String -> String -> Float
functionOne [] [] = 0
functionOne x y = (countNotXinY + countNotYinX) / (sizeX + sizeY)
    | where

{--
countCharsInString :: String -> String -> Int
countCharsInString string1 string2
    | string1 == string2 = length(string2)
    | otherwise = countFunction string1 string2 "" 0
        where 
            countFunction (x:xs) ys diffChars counter
                | xs == [] = counter
                | (((x `elem` ys) == True ) && ( (x `elem` diffChars) == False) ) == True = countFunction xs ys diffChars (counter+1)
                | otherwise = countFunction xs ys diffChars counter

countCharsInString :: String -> String -> Int
countCharsInString _ [] = 0
countCharsInString [] _ = 0
countCharsInString (x:xs) ys
    | (x `elem` ys) == True  = 1 + (countCharsInString xs (filter (==x) ys) )
    | otherwise = countCharsInString xs ys
    --}

countCharsNotInString :: String -> String -> Int
countCharsNotInString _ [] = 0
countCharsNotInString [] _ = 0
countCharsNotInString (x:xs) ys
    | (x `elem` ys) == True  = countCharsNotInString xs ys
    | otherwise = 1 + (countCharsNotInString xs (filter (==x) ys) )