{--
Write a function that, given a string, validates the string as a Finnish IBAN code.

For details, see https://en.wikipedia.org/wiki/International_Bank_Account_Number#Validating_the_IBAN

Length of a Finnish IBAN code is 18. Finnish IBAN begins with the country code FI and the rest of the characters are digits. 
You can assume that the input is without whitespaces.

Returns:
    -> Valid Finnish IBAN code = True
    -> Invalid Finnish IBAN code = False

Test cases:
    -> Valid
        - ibanFinnishCodeIsValid "FI2112345600000723"
    -> Invalid
        - ibanFinnishCodeIsValid "FI21123456000007"
        - ibanFinnishCodeIsValid "IT2112345600000723"
        - ibanFinnishCodeIsValid "FI211234560AS000723" 
--}

containOnlyDigits :: String -> Bool
containOnlyDigits [] = False
containOnlyDigits (x:xs)
    | xs == [] = True
    | ((fromEnum(x) < 47 || fromEnum(x) > 58) == True ) = False
    | otherwise = containOnlyDigits xs

checkIfIbanCountryIsValid :: String -> Bool
checkIfIbanCountryIsValid ibanCountryCode
    | (ibanCountryCode == "FI") = True
    | otherwise = False

ibanFinnishCodeIsValid :: String -> Bool
ibanFinnishCodeIsValid [] = False
ibanFinnishCodeIsValid ibanCode
    | ((length(ibanCode)) /= 18) = False
    | ((checkIfIbanCountryIsValid (take 2 ibanCode))== False) = False
    | ((containOnlyDigits (drop 2 ibanCode)) == False ) = False
    | otherwise = True