-- Student: João Paulo Nunes Soares
{--
Write a function that, given a list of numbers, produces a list with all elements
of the input list such that the element is followed by a greater number in the 
input list (the next number is greater).

The numbers in the output should be in the same order as in the input.

For example, if the input is [0,5,2,3,2,2,3,1] then the output should be [0,2,2]

Example of test cases: 
    - checkInList [0,5,2,3,2,2,3,1]
    - checkInList [0,5,10,3,2,2,3,7]

--}

checkInList :: [Int] -> [Int]
checkInList numberList = [ numberAnalyzed | (numberAnalyzed, nextNumber) <- zip numberList (drop 1 numberList), numberAnalyzed < nextNumber ]