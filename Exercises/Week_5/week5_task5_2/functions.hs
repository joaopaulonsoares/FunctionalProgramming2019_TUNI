module W5T2Functions where

import W5T2DataDefinitions

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