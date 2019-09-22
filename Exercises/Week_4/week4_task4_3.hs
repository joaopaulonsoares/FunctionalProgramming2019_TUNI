{--
Implement a readPhone function (String -> String -> String -> Phone) as follows:

The function
a) reads the phone type from the first string.

b) reads the country code code from the second string in the following way:
1. if the code has a '+' or "00" in the front, remove them
2. check that the code exists in a predefined list of country codes (you may define this as just a list of strings or a list of integers in your program)
3. read an integer out of the remaining string
4. call the function(that checks that the integer is >= 0) you created in Task4.2 with the integer to create the value for CountryCode.

c) reads the phone number from the third string by reading it as an integer and then calling the function you created in Task4.2.

If the input is correct, create a telephone number. Else, call error to throw an exception. Note that the read function or your functions from Task4.2 may also generate an exception.

Note: In this exercise do not try to create a custom Read instance, because it is probably unnecessarily hard at this point.

Note: Make sure not to import your answer for Task4.2 in your answer for this task, since in peer-reviewing the file will not be present. (You will need to copy-paste)

-> TEST CASES
    - readPhone "Other" "+358" "123456789"
    - readPhone "Other" "0008" "123456789"
    - readPhone "Other" "00358" "123456789"
    - readPhone "PrivateMobile" "+358" "123456789"
--}
-- ================= EXERCISE 4_2 FUNCTIONS =====================
data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other
    deriving(Show, Read, Eq)

-- Country Code
data CountryCode = CountryCode Integer deriving(Eq)
instance Show CountryCode where
    show (CountryCode n) =  "+" ++ show n 

fCountryCode :: Integer -> CountryCode
fCountryCode countryCode
    | countryCode < 0 = error "Country Code negative"
    | otherwise = CountryCode countryCode

-- Phone Number
data PhoneNo = PhoneNo Integer deriving(Eq)
instance Show PhoneNo where
    show (PhoneNo pn) = show pn

fPhoneNo :: Integer -> PhoneNo
fPhoneNo phoneNo
    | phoneNo < 0 = error "Phone Number negative"
    | otherwise = PhoneNo phoneNo

-- Phone
data Phone = Phone{ 
    phoneType :: PhoneType, 
    countryCode :: CountryCode, 
    phoneNo :: PhoneNo
} deriving(Eq)
instance Show Phone where
    show (Phone pt cc pn) = show cc ++ " " ++ show pn ++ " " ++ "(" ++ show pt ++ ")"   

-- ================================= exercise 3 =========================
available_countries = [358,55,100,1,2,3]

readPhone :: String -> String -> String -> Phone
readPhone pt cc pn
    | elem ccNumber available_countries = createPhoneNumber (read pt :: PhoneType) ccNumber (read pn :: Integer)
    | otherwise = error "Error. Phone can't be created. (Country code not in available list)"
    where createPhoneNumber pt2 cc2 pn2 = Phone pt2 (fCountryCode cc2) (fPhoneNo pn2)
          ccNumber = transfCountryCode cc

transfCountryCode :: String -> Integer
transfCountryCode (x:xs)
    | x == '+' = read xs :: Integer
    | otherwise = read (x:xs) :: Integer
