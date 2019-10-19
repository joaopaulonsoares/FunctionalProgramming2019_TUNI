{--
Change readPhone function in 5.1 to use readMaybe (given at https://www.sis.uta.fi/~csjtn/fp/) to read the 
values from the strings (ie do not check for empty strings, you will get Nothing from the readMaybe in that case anyway).

Hints: (Use of these hints is not necessary)
-You can append the name of the value constructor to the string in the function call of readMaybe to make use of the derived 
Read instance. (But then you might construct negative values)
-You can make your own typeclass, which has a read-like function and then make use of the readMaybe function in the 
instance declarations.
-You can make a polymorphic function that takes a function, which is used to constuct the value.]

-> TEST CASES
    - readPhone "Other" "+358" "123456789"
    - readPhone "Other" "0008" "123456789"
    - readPhone "Other" "00358" "123456789"
    - readPhone "PrivateMobile" "+358" "123456789"
--}

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving(Show, Read, Eq)

newtype CountryCode  = MakeCountryCode Integer deriving(Eq,Read)
newtype PhoneNo      = MakePhoneNo Integer deriving(Eq,Read)
data Phone        = MakePhone { phoneType   :: Maybe PhoneType
                                ,countryCode :: Maybe CountryCode
                                ,phoneNo     :: PhoneNo    
                                } deriving(Eq, Read)

availableCountryCodes = [358, 359, 41, 84]

toCountryCode :: Integer -> CountryCode
toCountryCode x | (x `elem` availableCountryCodes) == False = error "Invalid country code."
                | otherwise = MakeCountryCode x
                    
toPhoneNo :: Integer -> PhoneNo
toPhoneNo x | x < 0 = error "Invalid phone number."
            | otherwise = MakePhoneNo x
                
toPhone :: Maybe PhoneType -> Maybe CountryCode -> PhoneNo -> Phone
toPhone pt cc pn = MakePhone pt cc pn

-- Type constructor 

instance Show CountryCode where
    show (MakeCountryCode x) = '+' : show x
  
-- Type constructor 
instance Show PhoneNo where
    show (MakePhoneNo x) = show x

instance Show Phone where
  show phone = show cc ++ " "++ show pn ++ " (" ++ show pt ++ ")"
    where
      cc = case (countryCode phone) of
           Nothing ->  Nothing
           Just x -> countryCode phone
      pn = phoneNo phone
      pt = case (phoneType phone) of
           Nothing -> Nothing
           Just x -> phoneType phone 

removePrefix :: String -> String
removePrefix ('+':xs) = xs
removePrefix ('0':'0':xs) = xs
removePrefix xs = xs

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

readPhone :: String -> String -> String -> Phone
readPhone ptStr ccStr pnStr = toPhone pt cc pn
    where pt = (readMaybe ptStr :: Maybe PhoneType)
          pn = (toPhoneNo $ read pnStr)
          cc =
            let x = (readMaybe (removePrefix ccStr) :: Maybe Int)
            in case x == Nothing of
              True  -> Nothing
              False -> (Just(toCountryCode $ read (removePrefix ccStr)))