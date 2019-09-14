-- Student: João Paulo Nunes Soares
{--
Write a function that, given a list of strings and a character evaluates 
to a list with all the strings of the input list that either begin or end 
with the input character.

Variables:
    - wordsList = list of words in the format ["example1", "example2"]
    - charAv = character used in the comparison

A list of arbitrary points is been created from the point(0,0) to (100,100)

Example of test cases: 
    - functionEvaluateCharInWord ["teste", "today", "oil", "paulo", "joao"] 'o'
    - functionEvaluateCharInWord ["egg", "tampere", "helsinke", "finland", "brazil"] 'e'
--}

functionEvaluateCharInWord :: [String] -> Char -> [String]
functionEvaluateCharInWord wordsList charAv = [word | word <- wordsList, head word == charAv || last word == charAv]