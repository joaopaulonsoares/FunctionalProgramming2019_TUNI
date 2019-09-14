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