
myFunction :: String -> String -> String
myFunction _ [] = "[INVALID] Empty parameter"
myFunction [] _ = "[INVALID] Empty parameter"
myFunction string1 string2
    | string1 == string2 = string1
    | otherwise = compareStrings string1 string2 ""
        where
            compareStrings (x:xs) (y:ys) subString
                | xs == [] || ys == [] = subString
                | x /= y = compareStrings xs (y:ys) subString
                | otherwise = compareStrings xs ys subString 