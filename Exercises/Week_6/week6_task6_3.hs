{--
6.3 Make a program that reads repeatedly lines from terminal (that is a program you need to compile). If the line is follows the format of one of the three lines below:
Int ‘+’ Int
Int ‘-‘ Int
Int ‘*’ Int
then calculate the result of the arithmetic operation. Otherwise output an error message, like “I cannot calculate that” or something. Stop when the user types “quit”
You can use the prompt you like. putStr prints a string without the line change. A possible execution:
calc> 3 + 5
8
calc> a + 3
I cannot calculate that
calc> 4 - 3
1
calc> quit
bye

 => INSTRUCTIONS
  - Compile the file: ghc week6_task6_3.hs
  - Execute the main file

=> TEST CASES
  - 1 + 1
    = 2
  - 2 * 3
    = 6
  - 5 - 2
    = 3
  - random things
    = "I cannot calculate that"
  - quit
    = "bye..."


--}


import Data.Char
import System.IO
import System.Random
import qualified Data.ByteString.Lazy as B  
import qualified Data.ByteString as S  
import System.Environment  
import System.IO.Error

main = do   
    line <- getLine  
    if line == "quit"  
        then putStrLn "bye..." 
        else do
            let command = convertInput line
            if (validateCommand command) == True
                then
                    print (solveOperation command)
                else     
                    print "I cannot calculate that"   
            
            main

validOperations = ["+","-","*"]

validateCommand :: [String] -> Bool
validateCommand command
    | length command < 3 = False
    | otherwise = validateCommand' command
    where 
        validateCommand' (int1:op:int2:rest)
            | ((onlyDigits int1) && (op `elem` validOperations) && (onlyDigits int2)) == True = True
            | otherwise = False


digits = ['0'..'9']
onlyDigits :: String -> Bool
onlyDigits "" = False
onlyDigits (c:"")
    | c `elem` digits = True
    | otherwise = False
onlyDigits (c:cs) 
    | c `elem` digits = onlyDigits cs
    | otherwise = False

convertInput :: String -> [String]
convertInput [] = []
convertInput te@(x:xs) | x==' ' || x=='\t' || x=='\n' = convertInput xs
                    | otherwise                = a : convertInput b
    where
    (a, b) = break isSpace te
 
solveOperation :: [String] -> Int
solveOperation (int1:op:int2:rest)
    | op == "+" = (convertStringToInt int1) + (convertStringToInt int2)
    | op == "-" = (convertStringToInt int1) - (convertStringToInt int2)
    | op == "*" = (convertStringToInt int1) * (convertStringToInt int2)
    | otherwise = error "Invalid command"

convertStringToInt :: String -> Int
convertStringToInt x = read x::Int