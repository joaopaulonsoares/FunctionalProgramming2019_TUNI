-- Student: João Paulo Nunes Soares
{--
We represent playing cards with (Char,Int) pairs. 's' means spades, 'h' hearts, 'c' clubs and  'd' diamonds, with number values going from 2 to 14 (Ace being 14). Consider a game where a player gets two cards, and your function needs evaluate how many credits the player wins. The credits are determined as follows:

If the player has the Ace of Spades, that is ('s',14) then the player wins 14 credits. 
Otherwise if the player has  two consecutive numbes of the same suit (spades, hearts, clubs or diamonds), then the player wins 8 credits.
Otherwise if the player has a pair (same number values), then the player wins 6 credits.
Otherwise if the player has two consecutive numbers, then the player wins 4 credits.
Otherwise if the player has two cards of the same suit (spades, hearts, clubs or diamonds) then the player wins 2 credits.
Otherwise, the player wins 0 credits.

OBS:
    - If some information is not valid, an exception is thrown.

Example of test cases: 
    - countCredits ('s',14) ('d',11)
    - countCredits ('a',5) ('s',4)
    - countCredits ('c',8) ('c',7)

--}

countCredits :: (Char,Int) -> (Char,Int) -> Int
countCredits cardOne cardTwo
    | ((elem (fst cardOne) ['s','h','c','d'] ) == False) || ((elem (fst cardTwo) ['s','h','c','d'] ) == False)  = error "Not a valid suit"
    | ((snd cardOne < 2 || snd cardOne >14) || (snd cardTwo < 2 || snd cardTwo >14) ) = error "Not a valid number"
    | (cardOne == ('s',14)) || (cardTwo == ('s',14)) = 14
    | ((fst cardOne) == fst(cardTwo)) && ( (snd cardOne == succ(snd cardTwo) ) || (snd cardTwo == succ(snd cardOne)) ) = 8
    | snd cardOne == snd cardTwo = 6
    | (snd cardOne == succ(snd cardTwo) ) || (snd cardTwo == succ(snd cardOne)) = 4
    | fst cardOne == fst cardTwo = 2
    | otherwise = 0