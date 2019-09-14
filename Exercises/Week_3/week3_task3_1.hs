{--
Let us number the smaller case characters from 'a' to 'z' with numbers starting from 1, that is, 'a' is given number 1, 'b' is given number 2, etc.

Give functions to compute the following:
- characters that have an odd number. This should return "acegikmoqsuwy".
- characters that have a number that is a product of two odd positive integers x and y, where x/=1 and y/=1. This should return "iouy".

Note: Your functions needs to actually compute these values.

--}

computeFunctionOne :: Char -> String
computeFunctionOne charComputed
    | mod ((fromEnum charComputed) - 96) 2 /= 0 = "acegikmoqsuwy"
    | otherwise = "Don't have odd number"


computeFunctionTwo :: Char -> String
computeFunctionTwo charComputed
    | charNumber == 1 = "Not a product of two odd positive integers where x/=1 and y/=1" -- Even number -> 1 * 1
    | mod charNumber 2 == 0 = "Not a product of two odd positive integers where x/=1 and y/=1" -- Even number -> even * even or even* odd
    | (isPrime charNumber == True) = "Not a product of two odd positive integers where x/=1 and y/=1" -- Prime number -> 1 * number
    | otherwise = "iouy" -- Odd product of odd * odd
    where charNumber = ((fromEnum charComputed) - 96)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n | (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 = False
            | otherwise = True