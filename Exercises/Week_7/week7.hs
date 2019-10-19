-- ====================================== Event functions ============================================

data EventInfo = EventInfo { name :: String
                           , place :: String
                           , date :: Date
                           } deriving(Eq)

instance Show EventInfo where
  show (EventInfo n p d) =  "Event " ++ show n ++ " happens at "  ++ show p ++ " on "  ++ show d

makeEvent :: String -> String -> [Integer] -> EventInfo
makeEvent name place date = EventInfo name place (makeDate3 date)

{--
makeEvent :: String -> String -> [Integer] -> IO EventInfo
makeEvent name place date = do
        let event = EventInfo name place (makeDate3 date)
        putStrLn "ok"

        return (event) 
--}
-- ====================================== Date functions ======================================

data Date = Date { year :: Year, month :: Month, day :: Day } deriving (Eq, Ord, Show)
--instance Show Date where
--    show (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d  

data Month = MakeMonth Integer deriving (Eq, Ord, Show)
--instance Show Month where
--    show (MakeMonth x) = show x

-- If all values are ok, it is enough to call "MakeMonth x"
-- Now the input is an Integer. What if it was a String?
-- - then MakeMonth (read x :: Integer) would do it
toMonth               :: Integer -> Month
toMonth x = MakeMonth x

fromMonth             :: Month -> Integer
fromMonth (MakeMonth i) = i  -- Pattern match i out 

-- This is done similarly as Month
data Day = MakeDay Integer deriving (Eq, Ord, Show)
--instance Show Day where
--    show (MakeDay x) = show x

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

-- A function to check if a year is a leap year

leapYear (MakeYear y)
    | mod y 400 == 0 = True
    | mod y 100 == 0 = False
    | mod y 4 == 0 = True
    | otherwise = False

-- 3: Write a function to check if a given date (y,m,d) is correct
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

-- ====================================================================================================
standart_system_message = "I do not understand that. I understand the following:\n*Event <name> happens at <place> on <date>\n*Tell me about <eventname>\n*What happens on <date>\n*What happens at <place>\n*Quit"

main = loop $ return []

loop :: IO [EventInfo] -> IO ()
loop ioEvents =
 do
 input <- getLine
 if input == "Quit"
   then putStrLn "bye"
   else doCommand input ioEvents    
--Event 'Event' happens at 'Place' on '2019-10-08'
--Event 'Joao' happens at 'Place' on '2019-10-08'
--Event 'Joao' happens at 'Place' on '2019-10-50'

doCommand :: String -> IO [EventInfo] -> IO ()
doCommand input ioEvents = do
    events <- ioEvents --Now you can use events as [EventInfo]
    let input' = convertInput input

    if (events == [])
        then putStrLn "Vazio"
        else putStrLn "Preenchido"

    --putStrLn $ show (events)

    if(length input' < 3) 
        then do
            putStrLn standart_system_message
            loop $ return events
        else do
            case (input' !! 2) of
                "happens"-> do
                    let dateInInt = convertDateToInt (input' !! 6)
                    let validDate = correctDate (dateInInt !! 0) (dateInInt !! 1) (dateInInt !! 2)

                    if(validDate == True)
                        then do -- Valid Date
                            let eventName = (filter (not . (`elem` "'")) (input' !! 1) )
                            let eventPlace =  (filter (not . (`elem` "'")) (input' !! 4) )
                            let newEvent = addNewEvent2 eventName eventPlace (input' !! 6) events
                            putStrLn "ok"
                            loop $ return newEvent
                        else do -- Bad Date
                            putStrLn "Bad date"
                            loop $ return events
                "about" -> do
                    let teste = findEventsByName (input' !! 3) events
                    if (teste == []) 
                        then putStrLn "I do not know of such event"
                        else putStrLn $ show (teste)
                    
                    loop $ return events
                "on" -> do
                    putStrLn "on"
                "at" -> do
                    putStrLn "at"
                otherwise -> do
                    putStrLn standart_system_message
            {--
            if (input' !! 0 == "Event") 
                then do                        
                    let newEvent = addNewEvent2 (input' !! 1) (input' !! 4) (input' !! 6) events
                    --possiblyChangedEvents <- newEvent
                    putStrLn "ok"
                    loop $ return newEvent
        
                else do
                    possiblyChangedEvents <- ioEvents
                    loop $ return possiblyChangedEvents
            
            case (input' !! 2) of
                "on" -> putStrLn "oi finded"
                "at" -> putStrLn "at finded"
                "about" -> putStrLn "about finded"
                "happens" -> putStrLn "happens finded"
                _ -> putStrLn "erro"--}
            
            --possiblyChangedEvents <- ioEvents
            loop $ return events

    

addNewEvent :: String -> String -> String -> [EventInfo]-> [EventInfo]
addNewEvent eName ePlace eDate oldEvents = oldEvents++[makeEvent eName ePlace (convertDateToInt eDate)]

addNewEvent2 :: String -> String -> String -> [EventInfo]-> [EventInfo]
addNewEvent2 eName ePlace eDate oldEvents = 
    let newEntry = makeEvent eName ePlace (convertDateToInt eDate)
        validDate = correctDate (cDate !! 0) (cDate !! 1) (cDate !! 2)
    in case validDate of
        True -> oldEvents++[newEntry]
        False -> oldEvents
    where cDate = convertDateToInt eDate

findEventsByName :: String -> [EventInfo]-> [EventInfo]
findEventsByName nm ei = filter (\x -> name x == nm) ei

findEventsByPlace :: String -> [EventInfo]-> [EventInfo]
findEventsByPlace pl ei = filter (\x -> place x == pl) ei

{--
addNewEvent :: String -> String -> String -> [EventInfo]-> [EventInfo]
addNewEvent eName ePlace eDate oldEvents = do
    let cDate = convertDateToInt eDate

    --return oldEvents
    return (makeEvent eName ePlace cDate)
    
    -- Check if date is valid
    if ((correctDate (cDate !! 0) (cDate !! 1) (cDate !! 2)) == True) 
        then do
            --putStrLn("ok")
            let event = (makeEvent eName ePlace cDate)

            return event

        else do
            --putStrLn("Bad date")
            return (makeEvent eName ePlace [2019,08,10])--}


convertInput :: String -> [String]
convertInput command = words command 