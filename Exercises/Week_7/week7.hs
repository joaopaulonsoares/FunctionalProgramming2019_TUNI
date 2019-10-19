import Data.List.Split
import Data.Function (on)
import Data.List (sortBy)


-- ====================================== Event functions ============================================
newtype PlainString = PlainString String
instance Show PlainString where
    show (PlainString s) = s

data EventInfo = EventInfo { name :: String
                           , place :: String
                           , date :: Date
                           } deriving(Eq)

instance Show EventInfo where
  show (EventInfo n p d) =  "Event " ++ show(PlainString n) ++ " happens at "  ++ show(PlainString p) ++ " on "  ++ show d


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

data Date = Date { year :: Year, month :: Month, day :: Day } deriving (Eq, Ord)
instance Show Date where
    show (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d  

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
--Event 'Event G0' happens at 'Place' on '2019-10-08'
-- Event 'cEvent' happens at 'Place' on '2019-10-08'
--Event 'bJoao' happens at 'Place' on '2019-10-08'
--Event 'apaulo' happens at 'Place' on '2019-10-09'
--Event 'Joao' happens at 'Place' on '2019-10-50'

doCommand :: String -> IO [EventInfo] -> IO ()
doCommand input ioEvents = do
    events <- ioEvents --Now you can use events as [EventInfo]
    let input' = convertInput input
    let updatedEvents = []
--    if (events == [])
--        then putStrLn "Vazio"
--        else putStrLn "Preenchido"

    --putStrLn $ show (events)

    if(length input' < 3) 
        then do
            putStrLn standart_system_message
            --loop $ return events
        else do
            case (input' !! 0) of --- input 0
                "Event "-> do -- TODO caso o evento de mesmo nome já exista, substituir
                    let dateInInt = convertDateToInt (input' !! 5)
                    let validDate = correctDate (dateInInt !! 0) (dateInInt !! 1) (dateInInt !! 2)

                    if(validDate == True)
                        then do -- Valid Date
                            let eventName = (filter (not . (`elem` "'")) (input' !! 1) )
                            let eventPlace =  (filter (not . (`elem` "'")) (input' !! 3) )
                            let newEvent = addNewEvent2 eventName eventPlace dateInInt events
                            putStrLn "ok"
                            --updatedEvents <- newEvent

                            loop $ return newEvent
                        else do -- Bad Date
                            putStrLn "Bad date"
                            loop $ return events
                "Tell me about " -> do
                    let eventName = (filter (not . (`elem` "'")) (input' !! 1) )
                    let eventFinded = findEventsByName eventName events
                    if (length eventFinded == 1)
                        then putStrLn $ show (eventFinded !! 0)
                        else putStrLn "I do not know of such event"
                    
                    --loop $ return events
                "What happens on " -> do
                    let dateInInt = convertDateToInt (input' !! 1)
                    let validDate = correctDate (dateInInt !! 0) (dateInInt !! 1) (dateInInt !! 2)
                    let eventsFinded = findEventsByDate (makeDate3 dateInInt) events
                    
                    if(length eventsFinded == 0)
                        then putStrLn "Nothing that I know of"
                        else do
                            let eventsSorted = sortPersons eventsFinded
                            printElements eventsSorted

                "What happens at " -> do
                    let eventPlace = (filter (not . (`elem` "'")) (input' !! 1) )
                    let eventsFinded = findEventsByPlace eventPlace events

                    if (length eventsFinded == 0)
                        then putStrLn "Nothing that I know of"
                        else do
                            let eventsSorted = sortPersons eventsFinded
                            printElements eventsSorted

                otherwise -> do
                    putStrLn standart_system_message

            loop $ return events


printElements :: [EventInfo] -> IO()
printElements [] = return ()
printElements (x:xs) = do
    let message = "Event " ++ (pEventName x) ++ " happens at " ++ (pEventPlace x)
    putStrLn $ message
    printElements xs


-- Sort functions
comparing :: Ord b => (a -> b) -> a -> a -> Ordering
comparing = on compare

sortPersons :: [EventInfo] -> [EventInfo]
sortPersons = sortBy (comparing pEventName)

-- Record Syntax
pEventName :: EventInfo -> String
pEventName (EventInfo name _ _) = name    

pEventPlace :: EventInfo -> String
pEventPlace (EventInfo _ place _) = place

pEventDate :: EventInfo -> Date
pEventDate (EventInfo _ _ date) = date 

addNewEvent :: String -> String -> String -> [EventInfo]-> [EventInfo]
addNewEvent eName ePlace eDate oldEvents = oldEvents++[makeEvent eName ePlace (convertDateToInt eDate)]

addNewEvent2 :: String -> String -> [Integer] -> [EventInfo]-> [EventInfo]
addNewEvent2 eName ePlace eDate oldEvents = 
    let newEntry = makeEvent eName ePlace eDate
        validDate = correctDate (eDate !! 0) (eDate !! 1) (eDate !! 2)
    in case validDate of
        True -> oldEvents++[newEntry]
        False -> oldEvents
    --where cDate = convertDateToInt eDate

findEventsByName :: String -> [EventInfo]-> [EventInfo]
findEventsByName nm ei = filter (\x -> name x == nm) ei

findEventsByPlace :: String -> [EventInfo]-> [EventInfo]
findEventsByPlace pl ei = filter (\x -> place x == pl) ei

findEventsByDate :: Date -> [EventInfo]-> [EventInfo]
findEventsByDate dt ei = filter (\x -> date x == dt) ei

convertInput :: String -> [String]
convertInput command = splitOn "'" command 

--convertInput :: String -> [String]
--convertInput command = words command 

