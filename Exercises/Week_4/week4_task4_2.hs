{--
Task 4.2
Define the PhoneType as in Task 4.1.

Now, insted of using type synonyms, define data types CountryCode and PhoneNo so that both of them have a value constructor that takes an integer.
Derive instances for Eq for them and make Show instances for them so that:
CountryCode: print '+' in front of the number.
PhoneNo: print only the number.
Make a function for both of them that takes and Integer and throws an error if the integer is negative otherwise it creates the value.

Then again, using the record syntax, define Phone type for phone numbers that has only one value constructor with fields
phoneType :: PhoneType,
countryCode :: CountryCode, (This time a type of its own)
and phoneNo :: PhoneNo. (This time a type of its own)

Derive an instance for Eq for it, but for Show make it "pretty-print" the infromation in this form:
<country code><space><phone number><space><phone type in parenthesis>
e.g. +358 123456789 (WorkLandline)

Make a function of type
:: PhoneType
-> CountryCode (This time a type of its own)
-> PhoneNo (This time a type of its own)
-> Phone
but this time do not worry about the values of CountryCode and PhoneNo, since if they are created with the functions you made before they are already correct.

Note: Another option would be to use newtype keyword instead of data keyword for CountryCode and PhoneNo, but we will get to it later.

-> Test Cases (Based on the forum topic ("Example inputs and outputs for week 4 tasks") answear)
    OBS: This test cases are to be tested on GHCI

    - show $ function WorkLandline (fCountryCode (-358)) (fPhoneNo 123456789)
    - show $ function WorkLandline (fCountryCode 358) (fPhoneNo 123456789)
    - show $ function WorkLandline (fCountryCode 358) (fPhoneNo (-123456789))

--}

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

-- Main Function
function ::  PhoneType -> CountryCode -> PhoneNo -> Phone
function pt cc pn = Phone pt cc pn