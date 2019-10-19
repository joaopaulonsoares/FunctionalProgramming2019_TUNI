import Data.Char
import System.IO
import System.Random
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S
import System.Environment  
import System.IO.Error
import Data.List.Split
import Data.Function (on)
import Data.List (sortBy)

-- DATA.HS
data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving(Show, Read, Eq)

newtype CountryCode  = MakeCountryCode Integer deriving(Eq,Read)
newtype PhoneNo      = MakePhoneNo Integer deriving(Eq,Read)
data Phone           = MakePhone {phoneType   :: Maybe PhoneType
                                 ,countryCode :: Maybe CountryCode
                                 ,phoneNo     :: PhoneNo    
                                 } deriving(Eq, Read)

--You could have put these with functions aswell.
instance Show CountryCode where
  show (MakeCountryCode x) = '+' : show x

instance Show PhoneNo where
  show (MakePhoneNo x) = show x
  
instance Show Phone where
  show (MakePhone Nothing Nothing pn)     = show pn
  show (MakePhone Nothing (Just cc) pn)   = show cc ++ " "  ++ show pn
  show (MakePhone (Just pt) Nothing pn)   = show pn ++ " (" ++ show pt ++ ")"
  show (MakePhone(Just pt) (Just cc) pn)  = show cc ++ " "  ++ show pn ++ " (" ++ show pt ++ ")"


-- FUNCTIONS
availableCountryCodes = ["358", "359", "41", "84"]
toCountryCode :: Integer -> CountryCode
toCountryCode x 
  | x < 0 = error "Invalid country code."
  | not $ (show x) `elem` availableCountryCodes = error "Country code not available."
  | otherwise = MakeCountryCode x
                  
toPhoneNo :: Integer -> PhoneNo
toPhoneNo x 
  | x < 0 = error "Invalid phone number."
  | otherwise = MakePhoneNo x
              
toPhone ::   Maybe PhoneType
          -> Maybe CountryCode
          -> PhoneNo
          -> Phone
toPhone pt cc pn = MakePhone pt cc pn

readPhone :: String
          -> String 
          -> String
          -> Phone
readPhone ptStr ccStr pnStr = 
  let pt = read ptStr
      ccStr' = removePrefix ccStr
        where removePrefix ('+':xs) = xs
              removePrefix ('0':'0':xs) = xs
              removePrefix xs = xs
      cc = toCountryCode $ read ccStr'
      pn = toPhoneNo $ read pnStr
  in case (ptStr, ccStr, pnStr) of
    ("", "", _) -> toPhone Nothing Nothing pn
    ("",  _, _) -> toPhone Nothing (Just cc) pn
    ( _, "", _) -> toPhone (Just pt) Nothing pn
    ( _, _,  _) -> toPhone (Just pt) (Just cc) pn

-- ================

readMaybe :: (Read a) => String -> Maybe a  
readMaybe st = case reads st of [(x,"")] -> Just x  
                                _ -> Nothing 

handleError = return () --Not relevant

main :: IO ()
main = program Empty


program :: Phonebook -> IO ()
program pb = do
  input <- getLine
  doCommand pb $ words input

doCommand :: Phonebook -> [String] -> IO ()
doCommand pb ("find":[val]) =
  case findEntry val pb of
    --An entry was found.
    Node name phones _ _  -> do putStrLn $ name ++ " " ++ show (map show phones) --Phones in a list
                                program pb
    --There was no entry.
    Empty                 -> program pb
doCommand pb ("add":arg1:arg2:arg3:[arg4]) = do
  putStrLn "Done" 
  program $ addEntry arg1 arg2 arg3 arg4 pb
doCommand pb _ = handleError

-- =================== PHONE BOOK FUNCTIONS ===================
data Phonebook = Empty | Node String [Phone] Phonebook Phonebook deriving (Show,Read,Eq)

singleton :: String -> [Phone] -> Phonebook
singleton k ps = Node k ps Empty Empty

addEntry :: String -> String -> String -> String  -> Phonebook -> Phonebook
addEntry k p1 p2 p3 pb =
    let phone = readPhone p1 p2 p3      
        addEntry' k p Empty = singleton k [p]
        addEntry' k p (Node k' ps left right)
            | k==k'  = case elem (phoneNo p) $ map phoneNo ps of
                            False  -> Node k' (p:ps) left right
                            True   -> Node k ps left right
            | k<k'   = Node k' ps (addEntry' k p left) right
            | k>k'   = Node k' ps left (addEntry' k p right)
    in addEntry' k phone pb
    
findEntry :: String -> Phonebook -> Phonebook
findEntry x Empty = Empty
findEntry x (Node x' ps left right)
    | x==x' = singleton x ps
    | x<x'  = findEntry x left
    | x>x'  = findEntry x right
    
examplePhonebook =
    addEntry "PersonA" "WorkLandline"  "00358"  "123456789"
    $ addEntry "PersonA" "PrivateMobile" "358"    "123456789"
    $ addEntry "PersonB" "Other"         "+358"   "123456789"
    $ addEntry "PersonB" "PrivateMobile" "358"    "123456789"
    $ addEntry "PersonA" "WorkLandline"  "00358"  "123456789"
    $ addEntry "PersonA" "WorkMobile"    "358"    "123456789"
    $ addEntry "PersonD" "WorkLandline"  "+358"   "123456789"
    $ addEntry "PersonA" "WorkLandline"  "358"    "123456789"
    $ addEntry "PersonA" "WorkMobile"    "00358"  "123456789"
    $ addEntry "PersonA" "WorkMobile"    "358"    "987654321"
    $ addEntry "PersonB" "WorkLandline"  "358"    "2323"        
    $ addEntry "PersonB" "Other"         "+358"   "144"         
    $ addEntry "PersonC" "WorkLandline"  "358"    "12312123" Empty


-- add Maria WorkMobile +358 123457

tellAboutEventName :: String -> IO ()
tellAboutEventName teste = putStrLn "teste"

teste :: String -> [String]
teste c = splitOn "'" c

eventsSorted = sortBy (compare `on` place) events
