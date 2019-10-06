import qualified Data.Map as Map  


examplePhoneBook =
    addEntry "PersonA" "WorkLandline"  "00358"  "123456789" --This line is evaluated last, but addEntry adds to the head of the list so it is there as the first element (if it is added)
    $ addEntry "PersonA" "PrivateMobile" "358"    "123456789"
    $ addEntry "PersonB" "Other"         "+358"   "123456789"
    $ addEntry "PersonB" "PrivateMobile" "358"    "123456789"
    $ addEntry "PersonA" "WorkLandline"  "00358"  "123456789"
    $ addEntry "PersonA" "WorkMobile"    "358"    "123456789"
    $ addEntry "PersonD" "WorkLandline"  "+358"   "123456789"
    $ addEntry "PersonA" "WorkLandline"  "358"    "123456789"
    $ addEntry "PersonA" "WorkMobile"    "00358"  "123456789"
    $ addEntry "PersonA" "WorkMobile"    "358"    "987654321"
    $ addEntry "PersonB" "WorkLandline"  "358"    "2323"        --And so on..
    $ addEntry "PersonB" "Other"         "+358"   "144"         --Now this call gets the phone book that is constructed below.
    $ addEntry "PersonC" "WorkLandline"  "358"    "12312123" [] --This line is evaluated first


data PhoneBookEntry = PhoneBookEntry { name  :: String
                                     , phone :: Phone
                                    } deriving(Eq, Show)

type PhoneBook = [PhoneBookEntry]

findEntries :: String -> PhoneBook -> PhoneBook
findEntries nm pb = filter (\x -> name x == nm) pb


addEntry  :: String -> String -> String -> String -> PhoneBook -> PhoneBook
addEntry nm ptStr ccStr pnStr oldBook =
    let newEntry = PhoneBookEntry nm $ readPhone ptStr ccStr pnStr --Note that haskell is lazy and would not try to evaluate this if the name and number combo is already in the phone book. This might or might not be what you want to do.
        findPhoneNos = map phoneNo $  map phone $ findEntries nm oldBook
        numberCheck = (toPhoneNo $ read pnStr) `elem` findPhoneNos
    in case numberCheck of
    True  -> oldBook            -- <-Here the newEntry would not be evaluated.
    False -> newEntry : oldBook -- <-Here the newEntry would be evaluated.

--addEntryMap  :: String -> String -> String -> String -> PhoneBook -> PhoneBook
--addEntryMap nm ptStr ccStr pnStr oldBook =


-- ======================= Exercises week 4 ========================================
data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving(Show, Read, Eq)

data CountryCode  = MakeCountryCode Integer deriving(Eq,Read)
data PhoneNo      = MakePhoneNo Integer deriving(Eq,Read)
data Phone        = MakePhone {phoneType   :: PhoneType
                                ,countryCode :: CountryCode
                                ,phoneNo     :: PhoneNo    
                                } deriving(Eq, Read)

toCountryCode :: Integer -> CountryCode
toCountryCode x | x < 0 = error "Invalid country code."
                | otherwise = MakeCountryCode x
                    
toPhoneNo :: Integer -> PhoneNo
toPhoneNo x | x < 0 = error "Invalid phone number."
            | otherwise = MakePhoneNo x
                
toPhone :: PhoneType
            -> CountryCode
            -> PhoneNo
            -> Phone
toPhone pt cc pn = MakePhone pt cc pn

instance Show CountryCode where
    show (MakeCountryCode x) = '+' : show x

instance Show PhoneNo where
    show (MakePhoneNo x) = show x

instance Show Phone where
    show phone = show cc ++ " "++ show pn ++ " (" ++ show pt ++ ")" 
      where
        cc = countryCode phone
        pn = phoneNo phone
        pt = phoneType phone    

availableCountryCodes = ["358", "359", "41", "84"]
readPhone :: String -> String -> String -> Phone
readPhone ptStr ccStr pnStr = 
  let pt = read ptStr -- The derived Read instance makes the read function read a value out of string which is formatted the same as using show with derived instance for Show. (There could also be parentheses around it e.g. "(((WorkLandline)))" would read the same value as "WorkLandline")
      ccStr' = removePrefix ccStr --Note that the implementation of this function is right below inside the where clause.
        where removePrefix ('+':xs) = xs
              removePrefix ('0':'0':xs) = xs
              removePrefix xs = xs
      cc = toCountryCode $ read ccStr'
      pn = toPhoneNo $ read pnStr
  in case ccStr' `elem` availableCountryCodes of
    True  -> toPhone pt cc pn