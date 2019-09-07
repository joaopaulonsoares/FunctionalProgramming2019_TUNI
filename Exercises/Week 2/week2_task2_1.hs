{--
Write a function that, given a string, uses recursion on the list of characters and checks whether the string contains only 
digits or not. Empty string should return false.

Returns:
    - Only valid digits = True
    - Not only digits = False
    - Empty list = False
--}

containOnlyDigits :: String -> Bool
containOnlyDigits [] = False
containOnlyDigits (x:xs)
    | xs == [] = True
    | ((fromEnum(x) < 47 || fromEnum(x) > 58) == True ) = False
    | otherwise = containOnlyDigits xs