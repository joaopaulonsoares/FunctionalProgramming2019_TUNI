data Date = Date { year :: Year, month :: Month, day :: Day } deriving (Eq, Ord, Show)
--instance Show Date where
--    show (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d  

data Month = MakeMonth Integer deriving (Eq, Ord)
instance Show Month where
    show (MakeMonth x) = show x

-- If all values are ok, it is enough to call "MakeMonth x"
-- Now the input is an Integer. What if it was a String?
-- - then MakeMonth (read x :: Integer) would do it
toMonth               :: Integer -> Month
toMonth x = MakeMonth x

fromMonth             :: Month -> Integer
fromMonth (MakeMonth i) = i  -- Pattern match i out 

-- This is done similarly as Month
data Day = MakeDay Integer deriving (Eq, Ord)
instance Show Day where
    show (MakeDay x) = show x

toDay :: Integer -> Day
toDay x = MakeDay x

fromDay             :: Day -> Integer
fromDay (MakeDay i) = i 

newtype Year = MakeYear Integer deriving (Eq, Ord)
instance Show Year where
    show (MakeYear x) = show x

toYear :: Integer -> Year
toYear x = MakeYear x

fromYear :: Year -> Integer
fromYear (MakeYear x) = x

leapYear (MakeYear y)
    | mod y 400 == 0 = True
    | mod y 100 == 0 = False
    | mod y 4 == 0 = True
    | otherwise = False

correctDate :: Integer -> Integer -> Integer -> Bool
correctDate 0 _ _  = False
correctDate y m d
 | (elem m [1,3,5,7,8,10,12]) && (elem d [1..31]) = True
 | (elem m [4,6,9,11]) && (elem d [1..30]) = True
 | (m==2) && (elem d [1..28]) = True
 | (leapYear (toYear y)) && (m==2) && (d==29) = True
 | otherwise = False


splitDate :: Eq a => a -> [a] -> [[a]]
splitDate d [] = []
splitDate d s = x : splitDate d (drop 1 y) where (x,y) = span (/= d) s

makeDate2 :: String-> Date
makeDate2 date
    | correctDate yInt mInt dInt = Date { year = toYear yInt, month = toMonth mInt, day = toDay dInt }
    | otherwise = error "not correct combination of integers for year, month and day"
    where dateList = splitDate '-' (filter (not . (`elem` "'")) date )
          yInt = read (dateList !! 0) :: Integer
          mInt = read (dateList !! 1) :: Integer
          dInt = read (dateList !! 2) :: Integer

makeDate3 :: [Integer] -> Date
makeDate3 date = Date { year = toYear yInt, month = toMonth mInt, day = toDay dInt }
    where yInt = date !! 0
          mInt = date !! 1
          dInt = date !! 2

convertDateToInt :: String -> [Integer]
convertDateToInt date = [yInt,mInt,dInt]
    where dateList = splitDate '-' (filter (not . (`elem` "'")) date )
          yInt = read (dateList !! 0) :: Integer
          mInt = read (dateList !! 1) :: Integer
          dInt = read (dateList !! 2) :: Integer