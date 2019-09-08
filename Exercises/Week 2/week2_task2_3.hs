{--
We say that character pair (c1,c2) appears in string s with gap g, if c1 is before c2 and there are exactly g characters 
between c1 and c2 in s.
Write a function that, given a pair (c1,c2) a gap g, and a string s, returns an Int telling how many times (c1,c2) appear 
in s with gap g.
E.g. ('a','b') with gap 1 appears 2 times in “aaabbb".
Use recursion.

-> Test cases:
    - checkGap ('a','d') 2 "abcdabc"
    - checkGap ('a','b') 5 "aaabbbc"
    - checkGap ('a','d') 2 "abcdabc"
    - checkGap ('a','c') 2 "abcdabc"
    - checkGap ('a','c') 1 "abcdabc"
--}

checkGap :: (Char,Char) -> Int -> String -> Int
checkGap chars gap word = countTimes chars gap word 0
    where
        countTimes chars gap (x:xs) counter
            | (length(xs) <= gap ) = counter
            | xs == [] = counter
            | ( (fst chars) == x ) && ( (xs !! gap) == (snd chars) ) == True = countTimes chars gap xs (counter + 1)
            | otherwise = countTimes chars gap xs counter