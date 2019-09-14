-- Student: João Paulo Nunes Soares
{--
Write a function that given an Int x, evaluates to a list of such points in 
2-dimensional space  (pairs of type (Int,Int)), that their Manhattan distance 
from origin (0.0,0.0) is at most x. Use list comprehension.

Variables choosed
    - limitNumber = given an Int x
    - x = coordinate x
    - y = coordinate y

A list of arbitrary points is been created from the point(0,0) to (100,100)

Example of test cases: 
    - functionEvaluateList 10
    - functionEvaluateList 5
    - functionEvaluateList 15
--}

functionEvaluateList :: Int -> [(Int,Int)]
functionEvaluateList limitNumber = [ (x,y) | y <- [0..100], x <- [0..100], ((x - 0) + (y - 0) ) < limitNumber]