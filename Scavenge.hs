module Scavenge where
import Dictionaries
import Data.List (sort)
import Debug.Trace

--                                          Type Aliases
-- These type aliases help to abstract our code. 
-- 
type Hand = [Char]
type Move = String
type Play = [Move]
type Dictionary = [String] 

-- All undefined values and functions should be completed. Your code will compile and test 
-- (with the --test flag) even if some functions are left undefined.
--
--                                       Milestone
--

-- Digit Value
-- Calculates the value of each letter
letterValue :: Char -> Integer
letterValue c 
    | c `elem` "AEIOULNSTR" = 1
    | c `elem` "DG" = 2
    | c `elem` "BCMP" = 3
    | c `elem` "FHVWY" = 4
    | c `elem` "K" = 5
    | c `elem` "JX" = 8
    | c `elem` "QZ" = 10
    | otherwise = error "Element not proper letter"

-- Score takes a move and returns the sum of the letter values, according to the Scrabble scoring
-- system: https://scrabble.hasbro.com/en-us/faq
-- A helper function may be useful.
score :: Move -> Integer
score move = sum [letterValue c | c <- move]

-- scorePlay takes a play and returns the total score of all words.
scorePlay :: Play -> Integer
scorePlay play = sum [score move | move <- play]

removeItem :: Eq a => a -> [a] -> [a]
removeItem item [] = []
removeItem item (x:xs)
    | x == item = xs
    | otherwise = x:(removeItem item xs)

removeItems [] lst = lst
removeItems (x:xs) lst = removeItem x (removeItems xs lst)

remove :: Eq a => a -> [a] -> [a]   
remove item lst = if item `elem` lst then removeItem item lst else error "Not in list"

-- updateHand should take a hand (a list of characters), and a move (a string), and return the hand
-- that remains after that move is played.
updateHand :: Hand -> Move -> Hand
updateHand hand [] = hand
updateHand hand (x:xs) = updateHand (remove x hand) xs

-- canMake takes a hand and a move, and tells you if that move can be made with that hand. Be sure to
-- consider the possibility that a letter may occur more times in the move than it does in the hand.


canMake :: Hand -> Move -> Bool
--canMake hand [] = True
canMake hand move = length (removeItems move hand) == length hand - length move
    -- if x `elem` hand then canMake (removeItem x hand) xs else False

-- "DNAHTSET" `canMake` "HAND" = True 
-- "DNAHTSET" `canMake` "HAAND" = False
-- For full credit, this must run in near-linear time (n log n is sufficient)

-- isValidMove tests if a move is valid with respect to a dictionary and hand: 
-- the move must be a word in the dictionary and a move that can be made with the hand.
isValidMove :: Dictionary -> Hand -> Move -> Bool
isValidMove dict hand move = if move `elem` dict && canMake hand move then True else False
-- isValidMove tinyDict "MKEKIOCHAUX" "MAKE" = TRUE
-- isValidMove tinyDict "MKEKIOCHAUX" "MAXKE" = FALSE
-- isValidMove tinyDict "MKEKIOCHAUX" "FAKE" = FALSE

-- isValidPlay checks if a play is valid. Each move in the play must be a word in the dictionary, and
-- must be playable using whatever remains of the hand after all previous moves have been made.
isValidPlay :: Dictionary -> Hand -> Play -> Bool
isValidPlay dict hand [] = True
isValidPlay dict hand (x:xs) = if isValidMove dict hand x then isValidPlay dict (updateHand hand x) xs else False
-- isValidPlay tinyDict "TMAKE" ["TAKE"] = TRUE
-- isValidPlay tinyDict "TMAKE" ["MAKE"] = TRUE
-- isValidPlay tinyDict "TMAKE" ["TAKE","MAKE"] = False
    
-- validMoves: takes a dictionary and a hand, and returns all words that can be
-- created by letters in the hand. Order does not matter.
validMoves :: Dictionary -> Hand -> [Move]
validMoves dict hand = [word | word <- dict, canMake hand word]
-- validMoves shortDict "PEMDOVZIJM" = ["DIE","DO","I","ME","MOVE","MOVIE","PM"]

--                                  End of Milestone!


--                                  My Helpers
maxScore :: Play -> Play -> Play
maxScore x y = if (scorePlay x) > (scorePlay y) then x else y

findMaxScore :: [Play] -> Play
findMaxScore [] = []
findMaxScore [x] = x
findMaxScore (x:xs) = maxScore x (findMaxScore xs)

--                                  End of My Helpers

--                                  Core Project 

-- --- Brute Force Algorithms
-- You are going to search for the best play, instead of just choosing the best move one at a time.
-- To do so, you will consider every possible play, by considering every possible combination of
-- words. You will implement two variants. 
 
-- powerset: return the powerset of the input, i.e. the list of all sub-lists.
-- You may assume the list has no duplicates. 
-- The output should not have duplicates, up to sorting.
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = 
    let powers = powerset xs
    in [x:ps | ps <- powers] ++ powers
-- powerset [1,2] = [[],[1],[1,2],[2]]
-- It is acceptable to have [2,1] INSTEAD of [1,2], but not in addition.
-- length (powerset "abcde") = 32

-- The Naive Brute Force (naiveBrutePlay) takes every combination of moves in
-- the dictionary: the powerset of the dictionary. It will only work with very small
-- dictionaries, like tenWords.  You will then choose the best valid play out of that list.

naiveBrutePlay :: Dictionary -> Hand -> Play
naiveBrutePlay dictionary hand =
    let valids = validMoves dictionary hand
        sets = powerset valids
    in findMaxScore [set | set <- sets, isValidPlay dictionary hand set]
    -- I accidentally made my brutePlay efficient before I made the smartPlay efficient
        

-- The Smart Brute Force approach realizes that we can restrict the size of the dictionary
-- before we compute the powerset. There are probably MANY moves in the dictionary that we can't
-- create at all! So, first find all the moves that can be made with this hand. Then, take the
-- powerset of that restricted list to create a list of all plays made up of those moves. Then
-- find the best valid play from that list.
smartBrutePlay :: Dictionary -> Hand -> Play
smartBrutePlay dictionary hand = naiveBrutePlay dictionary hand


-- --- Greedy Algorithm

calcGreedyPlay [] hand lst = lst
calcGreedyPlay valids hand lst = 
    let maxValid = snd (maximum valids)
        newHand = updateHand hand maxValid
        newMoves = validMoves [snd valid | valid <- valids] newHand
    in calcGreedyPlay [(score move, move) | move <- newMoves] newHand (maxValid:lst)

-- greedyPlay: choose the best move you can at any given point in time, then check to see what
-- other moves you can make. [String]
greedyPlay :: Dictionary -> Hand -> Play
greedyPlay dictionary hand = 
    let valids = [(score move, move) | move <- validMoves dictionary hand]
    in calcGreedyPlay valids hand []

-- --- Recursive Game-Tree Algorithm

-- Finally we will try a recursive strategy to find the best play. Even the smart brute force
-- fails for large hands: I can make a lot of moves, but not a lot of moves simultaniously. Thus
-- the list of all possible plays is large, but the list of all valid plays is still likely
-- small. 
-- For this algorithm, start with the list of valid moves. Then, for each move find the
-- best play for the remaining hand. Select the hand that leads to the best overall score, counting both
-- the score of the move and the score of the best remaining play.
bestPlay :: Dictionary -> Hand -> Play 
bestPlay dictionary hand = 
    let valids = validMoves dictionary hand
    in findMaxScore [valid:newPlay | valid <- valids, let newPlay = bestPlay valids (updateHand hand valid)]
    -- I think I did the pruning by d./efault accidentally
    --in snd (maximum [(scorePlay play, play) | valid <- valids, let play = calcAllPlays dictionary hand valids []])



