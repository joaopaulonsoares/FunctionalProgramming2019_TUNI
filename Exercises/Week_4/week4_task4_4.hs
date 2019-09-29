{--
4. Use the types and functions you have created in the previous tasks in this task.

Make a record that contains simple phone book entry information:
Name
Phone

Suppose we have a list of phone book entries. Make functions to:

-Find a list of entries by a name. (All entries where the name is the given one)

-Add a new entry, given a string for the name, the three strings for the phone like in Task 4.3 and the list of phone book entries to add the new entry to.
If there already exists an entry with the given name and the given number(phoneNo field in Phone), then make no change.

Note: Make sure not to import your answer for Task4.3 or Task4.2 in your answer for this task, since in peer-reviewing the file will not be present. (You will need to copy-paste)
--}

-- Phone
data PhoneBook = PhoneBook{ 
    name :: String, 
    phone :: Phone
} deriving(Eq)

type BookListType = [PhoneBook]
bookList = [PhoneBook]

addNewEntry :: String -> String -> String -> String -> BookListType
addNewEntry name pt cc pn
    | (nameExistsInList name []) == True = []
    | otherwise = []
   -- where addNewRegister = (PhoneBook ):bookList 

nameExistsInList :: String -> [PhoneBook] -> Bool
nameExistsInList newName (x:xs)
    | newName == (x name) = True
    | otherwise = nameExistsInList newName xs

--getOnlyNamesFromBookList :: [PhoneBook]

-- ================= EXERCISE 4_3 FUNCTIONS =====================
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
