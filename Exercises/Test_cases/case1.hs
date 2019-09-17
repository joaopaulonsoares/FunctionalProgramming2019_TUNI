import Data.List
import Data.Maybe
-- Goes through the first string character by character and compares them to all the characters in second string. 
-- If strings have a common character, prints the common character and deletes it and all the characters before it from both strings.
-- Please note, if you want the answer of the example given in the assignment, you need to call commonString "bbbad" "aabbccdd" 
commonString :: String -> String -> String
commonString _ [] = []
commonString [] _ = []
commonString (x:xs) y
        |length y == 0 = []
        |elemIndex x y /= Nothing = x:commonString xs (drop ((fromJust (elemIndex x y)) + 1) y)
        |otherwise = commonString xs y