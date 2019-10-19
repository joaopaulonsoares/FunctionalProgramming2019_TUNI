
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
        then putStrLn "bye.." 
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