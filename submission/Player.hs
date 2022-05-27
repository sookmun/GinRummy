-- | This is the file you need to implement to complete the assignment. Remember
-- to comment where appropriate, use generic types and have fun!

module Player where

import Parser.Parser -- This is the source for the parser from the course notes
import Rummy.Types   -- Here you will find types used in the game of Rummy
import Cards         -- Finally, the generic card type(s)
-- You can add more imports if you need them
import Data.List
import Parser.Instances
import qualified Data.Set 
import Data.Maybe
import Rummy.Rules
-- import Data.List.Unique


-- | This card is called at the beginning of your turn, you need to decide which
-- pile to draw from.
-- Pick card has multiple strategies to get the best possible decision. It checks if the card is able to make a meld.less than five or has a same rank as 
-- a card in the hand
pickCard :: ActionFunc
pickCard card score memory oponent hand = if (canMeld card hand) || (getRank card <= Five) || (sameRank hand card) then (Discard,"") else (Stock,"")

--Get combinations of meld then check if the newcard can form a meld or not
canMeld :: Card -> [Card] -> Bool
canMeld card hand = anyTrue ((isIn card) <$> (makeCombination (hand ++ [card])))
    
--Checks the list of cards if the Card is inside
isIn :: Card -> [Card] -> Bool
isIn _ []  = False
isIn card (x:xs) = if x == card then True else isIn card xs

--Checks if there is any card in the hand that is same rank as the card
sameRank :: [Card] -> Card -> Bool
sameRank [] c = False
sameRank (x:xs) c = if getRank x == getRank c then True else sameRank xs c

--get the maximum deadwood card
--check if the deadwood is <=10 then knock ==0 then gin else Drop
-- | This function is called once you have drawn a card, you need to decide
-- which action to call.
playCard :: PlayFunc
playCard cardx score memory hand = do
    let discardCard = maxi (getDeadwood hand) --find card to discard
    let dw = sum((cardPoints) <$> (getDeadwood hand)) - (cardPoints (toDeadwoodTwo discardCard))
    if dw <=0 then (Action Gin discardCard,"") else
        if  dw <=10 
            then (Action Knock discardCard,"")
            else (Action Drop discardCard,"")

--code refer from https://stackoverflow.com/questions/45917027/haskell-max-number-in-a-list
maxi :: [Meld] -> Card
maxi [Deadwood x] = x
maxi (Deadwood x: Deadwood y:xs) = maxi ((if getRank x >= getRank y then (Deadwood x) else (Deadwood y) ):xs)



-- | This function is called at the end of the game when you need to return the
-- melds you formed with your last hand.
-- fmap toMeld to each list of cards that are possible Melds to form Melds
-- The functionality of makeMelds is taken from this website refer at https://github.com/gladed/haskell-rummy/blob/master/src/lib/Rummy.hs
-- I change and implemented many of my own function for my game
makeMelds :: MeldFunc
makeMelds score memory hand= (toMeld <$> (filterMelds (makeCombination hand) [] [])) ++ getDeadwood hand


--possible combination of the hand
--use subsequences to get all possible combination and use sort_list to sort by length, suit and rankof list for easier choosing meld
makeCombination :: [Card] -> [[Card]] 
makeCombination hand = filterFold checkMeld $sort_list $sortSuitRank hand

--Sort_list Function is taken from tutorial. sort_listRank and sort)listSuit is inspired form Sort_List
sort_list :: [[Card]] -> [[Card]]
sort_list = sortBy (\xs ys -> compare (length ys) (length xs))

sort_listRank :: [Card] ->[Card]
sort_listRank = (sortBy (\xs ys -> compare (getRank xs ) (getRank ys)))

sort_listSuit :: [Card] -> [Card]
sort_listSuit = (sortBy (\xs ys -> compare (getSuit xs ) (getSuit ys)))

--Need to sort by rank and suit to get all the possible the combinations because the subsequences function is not a permutation
--Sort by suit first then sort by rank because you want to choose the possible straights first.
--You can form a 5 card straight meld vs 3/4 set
sortSuitRank :: [Card] ->[[Card]]
sortSuitRank hand = (subsequences (sort_listSuit hand)) ++ (subsequences (sort_listRank hand))

-- Checking that if it is a possible Meld
checkMeld :: [Card] ->Bool
checkMeld hand = checkLength hand && (checkSets hand || checkStraights hand)

-- Function refer at https://github.com/gladed/haskell-rummy/blob/master/src/lib/Rummy.hs
--check that is all has the same sets
checkSets :: [Card] -> Bool
checkSets [] = False
checkSets (Card _ x :xs)= all(\c -> getRank c == x) xs

 -- Function refer at https://github.com/gladed/haskell-rummy/blob/master/src/lib/Rummy.hs
 --use this website to get a rough idea on the game functions
--  ensure that the list are all predesessor of each other
checkStraights :: [Card] -> Bool
checkStraights [] = False
checkStraights [_] = True
checkStraights (a:b:rest) = 
    getSuit a == getSuit b &&
    pred (getRank b) == getRank a &&
    checkStraights (b:rest)

--checking the length of the hands
checkLength :: [Card] -> Bool
checkLength hand = if (length hand >= 3 && length hand < 6) then True else False 

-- Function refer at https://github.com/gladed/haskell-rummy/blob/master/src/lib/Rummy.hs
--checking if all suits are the same 
sameSuits :: [Card] -> Bool
sameSuits [] = False 
sameSuits (Card x _ :xs)= all(\c -> getSuit c == x) xs

--Taken from tutorial 8
filterFold :: (a -> Bool) -> [a] -> [a]
filterFold _ [] = []
filterFold func (x:xs) | func(x) = x:filterFold func xs
                       | otherwise = filterFold func xs

-- getting the deadwood
-- generate all combinations of the hand and filter out the melds the left over is the deadwood
--what ever that hand and list of melds has will be filtered out leaving cards that were never made into melds
getDeadwood :: [Card] -> [Meld]
getDeadwood hand = toDeadwoodTwo <$> (hand \\ (flatten (makeCombination hand)))

--Taken from tutorial
flatten :: [[a]] -> [a]
flatten = foldl (++) [] 

-- The function keeps track of the card that has been used to form meld in the tacking list. Then it checks if 
--  the next meld if tisa unique meld and has no same card that has been used.
-- I am able to do it from the start because it has been sorted by length suit and rank so it is optimal meld first
filterMelds ::[[Card]] -> [[Card]] -> [Card] ->[[Card]]
filterMelds [] ret tracking = ret
filterMelds (x:xs) ret tracking = do
    if length (x:xs) == 0 then ret else
        if (anySimilar x tracking) then
            filterMelds xs (x : ret) (x ++ tracking)
            else
                filterMelds xs ret tracking

--This function check if there any card that is similar to the list of card that has been made into meld
--it checks the length of the list of possible meld then it filter our any card that is  similar  to the list of Cards in the formed melds
-- if the length changes then there are  similar  card if not then is a unique meld
anySimilar :: [Card]->[Card] ->Bool
anySimilar meld lstofDone= if (length (meld\\lstofDone) /= (length meld) ) then False else True

--Taken from tutorial 8
lift :: Applicative f => (a -> b -> c) -> f a -> f b -> f c 
lift f a b = f <$> a <*> b

--function allFalse and anyTrue taken from tutorial
--checks if the list of Bolean is all True or is False
allFalse :: [Bool] -> Bool
allFalse = foldr (&&) True

anyTrue :: [Bool] -> Bool
anyTrue = foldr (||) False

--Checking the length of the resulting list of cards and patern matching them into Melds
--the rest of the child functions are patern matching the card list. For toThree and ToFour check if is a
--straight or is a set
toMeld :: [Card] -> Meld
toMeld hand 
    |length hand ==3 = toThree hand
    |length hand == 4 =toFour hand
    |length hand == 5 = toFive hand
    |otherwise = toDeadwood hand

toThree :: [Card] ->Meld
toThree [a,b,c]  
    | checkStraights [a,b,c] = (Straight3 a b c)
    | otherwise = (Set3 a b c)

toFour :: [Card] -> Meld
toFour [a,b,c,d]  
    | checkStraights [a,b,c,d] = (Straight4 a b c d)
    | otherwise = (Set4 a b c d)

toFive :: [Card] ->Meld
toFive [a,b,c,d,e] = Straight5 a b c d e

toDeadwood :: [Card] -> Meld
toDeadwood [a] = Deadwood a

toDeadwoodTwo :: Card -> Meld
toDeadwoodTwo = Deadwood 

-- functions to get the suit or rank
getSuit :: Card -> Suit
getSuit (Card suit _) = suit

getRank :: Card -> Rank
getRank (Card _ rank) = rank


-- The function space , spaces and string are taken from tutorial
space :: Parser Char
space = is ' '

spaces :: Parser()
spaces = (space >> spaces) ||| pure()

string :: String -> Parser String
string = traverse is 
--Parsers for card taken and refered from Pass sessions
spade :: Parser Suit
spade = string "Spade" >> pure Spade

heart :: Parser Suit
heart = string "Heart" >> pure Heart

diamond :: Parser Suit
diamond = string "Diamond" >> pure Diamond

club :: Parser Suit
club = string "Club" >> pure Club

suit :: Parser Suit
suit = spade ||| heart ||| diamond ||| club

ace :: Parser Rank
ace = string "Ace" >> pure Ace

two :: Parser Rank
two = string "Two" >> pure Two

three :: Parser Rank
three = string "Three" >> pure Three

four :: Parser Rank
four = string "Four" >> pure Four

five :: Parser Rank
five = string "Five" >> pure Five

six :: Parser Rank
six = string "Six" >> pure Six

seven :: Parser Rank 
seven = string "Seven" >> pure Seven

eight :: Parser Rank
eight = string "Eight" >> pure Eight

nine :: Parser Rank
nine= string "Nine" >> pure Nine
 
ten :: Parser Rank
ten = string "Ten" >> pure Ten

jack :: Parser Rank
jack = string "Jack" >> pure Jack

queen :: Parser Rank
queen = string "Queen" >> pure Queen

king :: Parser Rank
king = string "King" >> pure King

rank :: Parser Rank
rank = ace ||| two ||| three ||| four ||| five ||| six ||| seven ||| eight ||| nine ||| ten ||| jack ||| queen ||| king

card :: Parser Card
card = do 
    _ <-string "Card"
    spaces
    s <- suit
    spaces
    r <-rank
    pure $ Card s r

