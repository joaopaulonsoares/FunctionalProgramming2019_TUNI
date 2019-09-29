{--
Take task 4.3 (You can use the example solution) and make the following changes to it:
-Use newtype instead of data wherever you can. (Easiest is probably to check from the dates.hs example file how to use newtype.)
-Change the definition of Phone so that country code and phone type are optional using Maybe.
-Change the Show instance of Phone so that it does not show country code or phone type if they are Nothing.
-Make the readPhone function accept empty strings for phone type and country code. If they are empty make them Nothing.

-> Test Cases
   - readPhone "" "" "123456789"
   - readPhone "Other" "+358" "123456789"
   - readPhone "Other" "0" "123456789"
   - readPhone "" "" ""

-> PS: I was not able to remove the "Just" or "Nothing" from the show instance so when printing the 
Phone it looks like: "Just +358 123456789 (Just Other)"

--}

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving(Show, Read, Eq)

newtype CountryCode  = MakeCountryCode Integer deriving(Eq,Read)
newtype PhoneNo      = MakePhoneNo Integer deriving(Eq,Read)
data Phone        = MakePhone { phoneType   :: Maybe PhoneType
                               ,countryCode :: Maybe CountryCode
                               ,phoneNo     :: PhoneNo    
                                } deriving(Eq, Read)

toCountryCode :: Integer -> CountryCode
toCountryCode x | x < 0 = error "Invalid country code."
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


availableCountryCodes = ["358", "359", "41", "84"]

removePrefix :: String -> String
removePrefix ('+':xs) = xs
removePrefix ('0':'0':xs) = xs
removePrefix xs = xs
  
readPhone :: String -> String -> String -> Phone
readPhone ptStr ccStr pnStr
    | pnStr == "" = error "Empty Number"
    | ptStr == "" && ccStr == "" = toPhone (Nothing) (Nothing) pn
    | ptStr == "" && ccStr /= "" = toPhone (Nothing) cc pn
    | ptStr /= "" && ccStr == "" = toPhone pt (Nothing) pn
    | otherwise = toPhone pt cc pn
    where pt = (Just(read ptStr :: PhoneType))
          pn =  (toPhoneNo $ read pnStr)
          cc =
            let x = removePrefix ccStr
            in case x `elem` availableCountryCodes of
              True  -> (Just(toCountryCode $ read ccStr))
              False -> error "Unknown country code!"
        
--show $ toPhone (Just(Other)) (Just(toCountryCode(358))) (toPhoneNo 123456789)