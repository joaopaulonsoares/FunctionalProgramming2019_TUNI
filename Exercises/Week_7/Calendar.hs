import Data.List.Split
import Data.Function (on)
import Data.List (sortBy)


-- ====================================== Event functions and declarations ============================================
data EventInfo = EventInfo { name :: String
                           , place :: String
                           , date :: Date
                           } deriving(Eq)

instance Show EventInfo where
  show (EventInfo n p d) =  "Event " ++ show(PlainString n) ++ " happens at "  ++ show(PlainString p) ++ " on "  ++ show d


makeEvent :: String -> String -> [Integer] -> EventInfo
makeEvent name place date = EventInfo name place (makeDate3 date)

-- ====================================== Date functions ======================================
-- Available in Date.hs file in moodle

data Date = Date { year :: Year, month :: Month, day :: Day } deriving (Eq, Ord)
instance Show Date where
    show (Date y m d) = show y ++ "-" ++ show m ++ "-" ++ show d  

data Month = MakeMonth Integer deriving (Eq, Ord)
instance Show Month where
    show (MakeMonth x) = case (x<10) of
                         True -> "0"++show x
                         False -> show x

toMonth               :: Integer -> Month
toMonth x = MakeMonth x

fromMonth             :: Month -> Integer
fromMonth (MakeMonth i) = i  -- Pattern match i out 

-- This is done similarly as Month
data Day = MakeDay Integer deriving (Eq, Ord)
instance Show Day where
    show (MakeDay x) = case (x<10) of
                       True -> "0"++show x
                       False -> show x


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

makeDate3 :: [Integer] -> Date -- TODO TORNAR ESSA A FUNÇÃO PRINCIPAL DE MAKEDATE
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

--Event 'Event G0' happens at 'Place' on '2019-09-08'
-- Tell me about 'Event G0'
-- Event 'cEvent' happens at 'Place' on '2019-10-08'
--Event 'bJoao' happens at 'Place' on '2019-10-08'
--Event 'apaulo' happens at 'Place' on '2019-10-09'
--Event 'Joao' happens at 'Place' on '2019-10-50'

doCommand :: String -> IO [EventInfo] -> IO ()
doCommand input ioEvents = do
    events <- ioEvents --Now you can use events as [EventInfo]
    let input' = convertInput input
    let updatedEvents = events


    if(length input' ==  1) -- TODO avaliar necessidade real disso
        then do -- Invalid command, no suficient informations
            putStrLn standart_system_message
            loop $ return events
        else do
            case (input' !! 0) of --- input 0
                "Event "-> do -- TODO não retornar direto no case mas só no fim
                    let dateInInt = convertDateToInt (input' !! 5)
                    let validDate = correctDate (dateInInt !! 0) (dateInInt !! 1) (dateInInt !! 2)

                    if(validDate == True)
                        then do -- Valid Date
                            let eventName = (filter (not . (`elem` "'")) (input' !! 1) )
                            let eventPlace =  (filter (not . (`elem` "'")) (input' !! 3) )
                            let newEvent = addNewEvent2 eventName eventPlace dateInInt events

                            putStrLn "ok"
    
                            loop $ return newEvent
                        else do -- Bad Date
                            putStrLn "Bad date"
                            --loop $ return events
                            loop $ return events
    
                "Tell me about " -> do
                    let eventName = (filter (not . (`elem` "'")) (input' !! 1) )
                    let eventFinded = findEventsByName eventName events
                    if (length eventFinded == 1)
                        then putStrLn $ show (eventFinded !! 0)
                        else putStrLn "I do not know of such event"
                    
                    loop $ return events
                "What happens on " -> do
                    let dateInInt = convertDateToInt (input' !! 1)
                    let validDate = correctDate (dateInInt !! 0) (dateInInt !! 1) (dateInInt !! 2)
                    let eventsFinded = findEventsByDate (makeDate3 dateInInt) events
                    
                    if(length eventsFinded == 0)
                        then putStrLn "Nothing that I know of"
                        else do
                            let eventsSorted = sortEvents eventsFinded
                            printElementsDate eventsSorted
                    
                    loop $ return events

                "What happens at " -> do
                    let eventPlace = (filter (not . (`elem` "'")) (input' !! 1) )
                    let eventsFinded = findEventsByPlace eventPlace events

                    if (length eventsFinded == 0)
                        then putStrLn "Nothing that I know of"
                        else do
                            let eventsSorted = sortEvents eventsFinded
                            printElementsPlace eventsSorted
                    
                    loop $ return events

                otherwise -> do
                    putStrLn standart_system_message
                    loop $ return events
 
            --putStrLn "oi no final"
            --loop $ return events

    --putStrLn "oi no final"
    --loop $ return events


printElementsPlace :: [EventInfo] -> IO()
printElementsPlace [] = return ()
printElementsPlace (x:xs) = do
    let message = "Event " ++ (pEventName x) ++ " happens at " ++ (pEventPlace x)
    putStrLn $ message
    printElementsPlace xs

printElementsDate :: [EventInfo] -> IO()
printElementsDate [] = return ()
printElementsDate (x:xs) = do
    let message = "Event " ++ (pEventName x) ++ " happens on "
    putStrLn $ message ++ show (pEventDate x)
    printElementsDate xs

addNewEvent :: String -> String -> String -> [EventInfo]-> [EventInfo]
addNewEvent eName ePlace eDate oldEvents = oldEvents++[makeEvent eName ePlace (convertDateToInt eDate)]


addNewEvent2 :: String -> String -> [Integer] -> [EventInfo]-> [EventInfo] -- TODO TORNAR FUNÇÃO PRINCIPAL
addNewEvent2 eName ePlace eDate oldEvents = 
    let existsEntry = findEventsByName eName oldEvents
        newEntry = makeEvent eName ePlace eDate
    in case (length existsEntry == 0) of
        True -> oldEvents++[newEntry] -- don't exist already
        False -> (filter (\x -> name x /= eName) oldEvents)++[newEntry] -- exists and drop before add new entry

-- ================================ FUNCTIONS TO SEARCH FOR INFORMATIONS =================================

findEventsByName :: String -> [EventInfo]-> [EventInfo]
findEventsByName nm ei = filter (\x -> name x == nm) ei

findEventsByPlace :: String -> [EventInfo]-> [EventInfo]
findEventsByPlace pl ei = filter (\x -> place x == pl) ei

findEventsByDate :: Date -> [EventInfo]-> [EventInfo]
findEventsByDate dt ei = filter (\x -> date x == dt) ei

-- ========================================== AUXILIAR FUNCTIONS ==========================================

convertInput :: String -> [String]
convertInput command = splitOn "'" command 

-- Sort functions
comparing :: Ord b => (a -> b) -> a -> a -> Ordering
comparing = on compare

sortEvents :: [EventInfo] -> [EventInfo]
sortEvents = sortBy (comparing pEventName)

-- Record Syntax
pEventName :: EventInfo -> String
pEventName (EventInfo name _ _) = name    

pEventPlace :: EventInfo -> String
pEventPlace (EventInfo _ place _) = place

pEventDate :: EventInfo -> Date
pEventDate (EventInfo _ _ date) = date 

-- Print helper
newtype PlainString = PlainString String
instance Show PlainString where
    show (PlainString s) = s