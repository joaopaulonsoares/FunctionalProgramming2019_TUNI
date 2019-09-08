{--
Write a function that, given two strings s1 and s2, computes a common "substring" of s1 and s2 as follows. The function finds the 
earliest common character c (closest to head of either s1 or s2 and appearing in both sequences). The function removes c and all 
the characters before it in both strings, puts c in the output string, and continues. E.g. s1 = "aabbccdd", s2="bbbbad" -> "bbd”.

If there are two earliest common characters, you may pick either one, that is, from s1 or s2. Please note that the result is not 
what is normally meant by substring.

Use recursion.
--}
myFunction :: String -> String -> String
myFunction _ [] = ""
myFunction [] _ = ""
myFunction (x:xs) (y:ys)
    | x == y = x:(computeAfterFirstEqualChar xs ys)
    | otherwise = myFunction xs (y:ys)
        where
            computeAfterFirstEqualChar _ [] = ""
            computeAfterFirstEqualChar [] _ = ""
            computeAfterFirstEqualChar (x:xs) (y:ys)
                | x == y = x:(computeAfterFirstEqualChar xs ys)
                | otherwise = computeAfterFirstEqualChar xs ys