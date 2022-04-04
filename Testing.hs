module Testing where
import Scavenge
import Dictionaries
import Control.Monad.Extra
import Test.Grader.Core
import Test.Grader.Rubric
import Test.Grader.Eval
import Test.Grader.Tests
import Test.Grader.Parsing.Util
import Test.Grader.Parsing
import Data.List
import Control.Monad.Trans.RWS

testScore :: Grader String
testScore = assess "score" 2 $ do
    check "that score is defined" $ shouldBeDefined "score"
    checkpoint
    check "the score of KB" $ (score "KB") `shouldBe` 8
    check "the score of the empty string" $ (score "") `shouldBe` 0
    check "the score of QA" $ (score "QA") `shouldBe` 11
    check "the score of JF" $ (score "JF") `shouldBe` 12
    check "for an error on non-letter characters" $ shouldErr (score "K9X")

testScoreplay = assess "scoreplay" 3 $ do
            check "the score of \"KB\" \"QA\"" $ (scorePlay ["KB", "QA"]) `shouldBe` 19
            check "that the empty play has score zero" $ (scorePlay []) `shouldBe` 0

testRemove :: Grader String
testRemove = assess "remove" 5 $ do
    let lst3 = ["one", "two", "three"]
    check "that remove is defined" $ shouldBeDefined "remove"
    checkpoint
    check "that remove can remove \'a\' from \"ab\"" $ (remove 'a' ['a'..'b']) `shouldBe` ['b']
    check "that remove works on a list of three" $ (remove "three" lst3) `shouldBe` (init lst3)
    check "That remove only removes the first match when there are duplicates" $ 
        ((sort $ remove 1 [5,3,1,7,3,1,9,10,1]) `shouldBe` [1,1,3,3,5,7,9,10]) <> ((sort $ remove 'a' "abcra") `shouldBe` ("abcr"))
    check "for an error when the element isn't in the list" $ shouldErr (length $ remove 11 [1..10]) <> shouldErr (length $ remove 11 [])

testCanMake :: Grader String
testCanMake = assess "Testing canMake" 5 $ do
    let hand1 = "TESTHAND"
        hand2 = "DNAHTSET"
        hand3 = "AAABBBCCC"
    check "that canMake is defined" $ shouldBeDefined "canMake"
    checkpoint
    check "that any hand can make the empty move" $ "" `shouldSatisfy` (hand1 `canMake`)
    check "that \"TESTHAND\" and its reversal canMake \"TEST\" and \"HAND \"" $ 
        "TEST" `shouldSatisfy` (hand1 `canMake`) <> "TEST" `shouldSatisfy` (hand2 `canMake`) 
        <> "HAND" `shouldSatisfy` (hand1 `canMake`) <> "HAND" `shouldSatisfy` (hand2 `canMake`)
    check "that a hand canMake itself" $ hand3 `shouldSatisfy` (`canMake` "ABCABCABC")
    check "that \"TESTHAND\" cannot make \"HAAND\"" $ "HAAND" `shouldSatisfy` (not . (hand1 `canMake`))  
    check "that the empty hand cannot make nonempty moves" $ "EMPTY" `shouldSatisfy` (not . ("" `canMake`))

handOne = "ABCDEFG"
handTwo = "AAABBBCCC"
testUpdateHand :: Grader String
testUpdateHand = assess "updateHand" 5 $ do
    check "playing \"ADG\" from handOne" $ (sort $ updateHand handOne "ADG") `shouldBe` ("BCEF")
    check "playing \"CCBBAA\" from handTwo" $ (sort $ updateHand handTwo "CCBBAA") `shouldBe` ("ABC")
    check "removing letters from a hand with duplicates" $  (sort $ updateHand handTwo "ABC") `shouldSatisfy` (not . null)
    check "for an error when the hand cannot make the move" $ shouldErr (sort $ updateHand handOne "FAIL")

testIsValidMove :: Grader String
testIsValidMove = assess "isValidMove" 3 $ do
    let tf = isValidMove tinyDict
    check "that \"MAKE\" and \"I\" are valid moves" $ "MAKE" `shouldSatisfy` (tf "MKEKIOCHAOX") <> "I" `shouldSatisfy` (tf "MKEKIOCHAOX") 
    check "that moves that can't be made are invalid" $ "MAKE" `shouldSatisfy`(not . tf "MKKIOCHAOX" ) 
    check "that moves not in the dictionary are invalid" $ "MAKI" `shouldSatisfy`(not . tf "MKKIOCHAOX")

testIsValidPlay :: Grader String
testIsValidPlay = assess "isValidPlay" 4 $ do
    let hand = "THEQUICKBROWN"
        play1 = ["THE", "QUICK", "BROWN"]
        play2 = ["THE", "THE", "QUICK", "QUICK", "BROWN", "BROWN"]
        play3 = ["THEQUICKBROWN"]
    check "that \"THE QUICK BROWN\" is a valid play" $ play1 `shouldSatisfy` (isValidPlay standardDict hand)
    check "that plays with too many letters are invalid" $ play2 `shouldSatisfy` (not . isValidPlay standardDict hand) 
    check "that plays with invalid words are invalid" $ play3 `shouldSatisfy` (not . isValidPlay standardDict hand)

testValidMoves :: Grader String
testValidMoves = assess "validMoves" 4 $ do
    let hand1 = "PEMDOVZIJM"
        hand2 = "EIFYOZWFKC"
        hand3 = "AAJNURWLGG"
        moves1 = ["DIE","DO","I","ME","MOVE","MOVIE","PM"]
        moves2 = ["FEW","I","IF","KEY","OF","OFF","OFFICE","OK","WE","WIFE"]
        moves3 = ["A","GUN","LAW","RUN","WAR"]
    check "that validMoves the valid moves for \"PEMDOVZIJM\" with shortDict" $ (sort $ validMoves shortDict hand1) `shouldBe` moves1
    check "that validMoves finds the valid moves for \"EIFYOZWFKC\" with shortDict" $ (sort $ validMoves shortDict hand2) `shouldBe` moves2
    check "that validMoves finds the valid moves for \"AAJNURWLGG\" with shortDict" $ (sort $ validMoves shortDict hand3) `shouldBe` moves3
    check "that validMoves finds the valid moves for the empty list" $ (sort $ validMoves shortDict "") `shouldBe` []

testGreedyPlay :: Grader String
testGreedyPlay = assess "greedyPlay" 10 $ do
    let customDict = ["FIVE","IVE","OF"]
    check "that greedyPlay finds \"THEIR\" in \"THEIR\"" $ (greedyPlay tinyDict "THEIR") `shouldBe` ["THEIR"]
    check "CLOSEFLOOR on shortDict" $ (sort $ greedyPlay shortDict "CLOSEFLOOR") `shouldBe` ["FORCE", "SO"] 
    check "FIVEO on [FIVE, IVE, OF]" $ (sort $ greedyPlay customDict "FIVEO") `shouldBe` ["FIVE"] 
    check "six FIVEs on [FIVE, IVE, OF]" $ (sort $ greedyPlay customDict "FIVEFIVEFIVEFIVEFIVEFIVEO") `shouldBe` (replicate 6 "FIVE") 

testPowerset :: Grader String
testPowerset = assess "powerset" 8 $ do
    let sms lst = sort $ map sort lst
        lst1 = [1, 2] :: [Int]
        lst2 = ['a', 'b', 'c']
        lst3 = [] :: [Int]
        lst4 = [1..7] :: [Int]
        res1 = [[],[1],[1,2],[2]] :: [[Int]]
        res2 = ["","a","ab","abc","ac","b","bc","c"]
        res3 = [[]] :: [[Int]]
        res4 = [[],[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5],[1,2,3,4,5,6],[1,2,3,4,5,6,7]
                ,[1,2,3,4,5,7],[1,2,3,4,6],[1,2,3,4,6,7],[1,2,3,4,7],[1,2,3,5],[1,2,3,5,6],[1,2,3,5,6,7]
                ,[1,2,3,5,7],[1,2,3,6],[1,2,3,6,7],[1,2,3,7],[1,2,4],[1,2,4,5],[1,2,4,5,6],[1,2,4,5,6,7],[1,2,4,5,7]
                ,[1,2,4,6],[1,2,4,6,7],[1,2,4,7],[1,2,5],[1,2,5,6],[1,2,5,6,7],[1,2,5,7],[1,2,6],[1,2,6,7],[1,2,7]
                ,[1,3],[1,3,4],[1,3,4,5],[1,3,4,5,6],[1,3,4,5,6,7],[1,3,4,5,7],[1,3,4,6],[1,3,4,6,7],[1,3,4,7],[1,3,5]
                ,[1,3,5,6],[1,3,5,6,7],[1,3,5,7],[1,3,6],[1,3,6,7],[1,3,7],[1,4],[1,4,5],[1,4,5,6],[1,4,5,6,7],[1,4,5,7]
                ,[1,4,6],[1,4,6,7],[1,4,7],[1,5],[1,5,6],[1,5,6,7],[1,5,7],[1,6],[1,6,7],[1,7],[2],[2,3],[2,3,4]
                ,[2,3,4,5],[2,3,4,5,6],[2,3,4,5,6,7],[2,3,4,5,7],[2,3,4,6],[2,3,4,6,7],[2,3,4,7],[2,3,5],[2,3,5,6],[2,3,5,6,7]
                ,[2,3,5,7],[2,3,6],[2,3,6,7],[2,3,7],[2,4],[2,4,5],[2,4,5,6],[2,4,5,6,7],[2,4,5,7],[2,4,6]
                ,[2,4,6,7],[2,4,7],[2,5],[2,5,6],[2,5,6,7],[2,5,7],[2,6],[2,6,7],[2,7],[3],[3,4],[3,4,5],[3,4,5,6],[3,4,5,6,7]
                ,[3,4,5,7],[3,4,6],[3,4,6,7],[3,4,7],[3,5],[3,5,6],[3,5,6,7],[3,5,7],[3,6],[3,6,7]
                ,[3,7],[4],[4,5],[4,5,6],[4,5,6,7],[4,5,7],[4,6],[4,6,7],[4,7],[5],[5,6],[5,6,7],[5,7],[6],[6,7],[7]] 
    check "that powerset is defined" $ shouldBeDefined "powerset"
    checkpoint
    check "the powerset of [1,2]" $ (sms $ powerset lst1) `shouldBe` (res1)
    check "the powerset of \"abc\"" $ (sms $ powerset lst2) `shouldBe` (res2)
    check "the powerset of the empty list" $ (sms $ powerset lst3) `shouldBe` (res3)
    check "the powerset of [1..7]" $ (sms $ map sort $ powerset lst4) `shouldBe` (res4)
    check "the length of the powerset of [1..7]" $ (length $ powerset lst4) `shouldBe` (length res4)
    shlem <- check "that powerset isn't even more exponential than needed" $ shouldNotRepeatWork "powerset"
    return $
      ifM allPassed (passWith 8)
        (ifM (failed shlem)
             (passWith 6)
             (failWith 0))

testNaiveBrutePlay :: Grader String
testNaiveBrutePlay = assess "naiveBrutePlay" 3 $ do
    let perfectDict = ["CLOSE", "FLOOR", "OTHER", "YOUR", "FORCE", "SO"]
        customDict = ["FIVE","IVE","OF"]
    check "on THEIR using dictionary [THEIR]" $ (naiveBrutePlay ["THEIR"] "THEIR") `shouldBe` ["THEIR"]
    check "when multiple words are needed" $ (sort $ naiveBrutePlay customDict "FIVEO") `shouldBe` ["IVE", "OF"]
    check "solves CLOSEFLOOR on bruteDict" $ (scorePlay $ naiveBrutePlay perfectDict "CLOSEFLOOR") `shouldBe` (score "CLOSEFLOOR")

testSmartBrutePlay :: Grader String
testSmartBrutePlay = assess "smartBrutePlay " 3 $ do
    let cTinyDict = tinyDict \\ ["IT"]
        customDict = ["FIVE","IVE","OF"] ++ [replicate k 'Z' | k <- [1..11]]
    check "on THEIR with tiny dictionary (removing IT)" $ (smartBrutePlay cTinyDict "THEIR") `shouldBe` ["THEIR"]
    check "when multiple words are needed" $ (sort $ smartBrutePlay customDict "FIVEO") `shouldBe` ["IVE", "OF"]
    check "solves CLOSEFLOOR" $ (scorePlay $ smartBrutePlay shortDict "CLOSEFLOOR") `shouldBe` (score "CLOSEFLOOR")

testGameTree :: Grader String
testGameTree = assess "bestPlay" 16 $ do
    let cTinyDict = tinyDict \\ ["IT"]
        otherTinyDict = ["CELL","CLOSE","FLOOR","FOR","FORCE", "THING", "THINK", "THIS",
                         "THOSE", "TIME", "TO", "TWO", "UP", "USE", "VERY", "WANT", "WAY", "WE", "WELL",
                         "WHAT", "WHEN", "WHICH", "WHO", "WILL", "WITH", "WOULD", "YEAR", "YOU", "YOUR"] 
    check "on THEIR with tiny dictionary (removing IT)" $ (bestPlay cTinyDict "THEIR") `shouldBe` ["THEIR"]
    check "solves CLOSEFLOOR with shortDict" $ (scorePlay $ bestPlay shortDict "CLOSEFLOOR") `shouldBe` (score "CLOSEFLOOR")
    check "solves triple CLOSEFLOOR with bestPlayDict" $ (scorePlay $ bestPlay otherTinyDict "CLOSECLOSECLOSEFLOORFLOORFLOOR") `shouldBe` (3*(score "CLOSEFLOOR"))
    check "solves many \"A\"s with shortDict" $ (scorePlay $ bestPlay shortDict "AAAAA") `shouldBe` (score "AAAAA")

genCM :: String -> Int -> (String, Hand)
genCM str n = 
    let strA = concatMap (replicate n) str
    in (strA, reverse strA)

genVW :: String -> Int -> (Dictionary, Hand)
genVW str n = 
    let sortedSD = sort shortDict
        strA = concatMap (replicate 4) str :: Hand
        shortIshDict = concatMap (replicate n) sortedSD :: Dictionary
    in  (shortIshDict, strA)

genPS :: (Num a, Enum a) => a -> [a]
genPS n = [1..n]

genGP :: String -> Int -> (Dictionary, Hand)
genGP str n = 
    let strA = concatMap (replicate n) str               
    in  (shortDict, strA)

testComplexities :: Grader String
testComplexities = assess "full credit: algorithm complexities" 10 $ do
       cm <- check "that canMake is linear" $ (uncurry canMake) `shouldBeLinearIn` (genCM "ABCDEFG")
       vm <- check "that validMoves is linear" $ (uncurry validMoves) `shouldBeLinearIn` (genVW "OTHERYOUR")
       ps <- check "that powerset is non-poly" $ powerset `shouldBeNonPolyIn` genPS
       gp <- check "that greedyPlay is polynomial" $ (uncurry greedyPlay) `shouldBePolyIn` (genGP "OTHERYOUR")
       return $ useWeights [(cm, 4), (vm, 4), (ps, 1), (gp, 1)]

tree :: Grader String
tree = describe "Project 2" $ do
    describe "milestone" $ do
      testScore
      testScoreplay
      testRemove
      testUpdateHand
      testCanMake
      checkpoint
      testIsValidMove
      testIsValidPlay
      testValidMoves
    checkpoint
    describe "core project" $ do
      testGreedyPlay
      describe "brute force algorithms" $ do 
        testPowerset
        testNaiveBrutePlay
        testSmartBrutePlay
      testGameTree
    checkpoint
    describe "full credit tests" $ do
      testComplexities

runTests :: Int -> Bool -> IO ()
runTests verb force = do
    let a = runGrader tree
    format <- makeFormat verb force "projectDesc.yaml"
    runRWST a () format 
    return ()
