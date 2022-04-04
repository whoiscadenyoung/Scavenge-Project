module Main where
import Data.Char
import Data.List
import System.Random
import System.Console.GetOpt
import System.Environment
import Control.Monad
import Data.Maybe
import System.Console.ANSI
import System.Exit
import Coloring
import Testing
import Scavenge
import Dictionaries
import Testing

-- Options record
data Options = Options {
   optHelp              :: Bool
 , optTest              :: Bool
 , handSize             :: Int
 , optForceTests        :: Bool
 , optDict              :: Int
 , optSolve             :: Int
 , verbocity            :: Int
 } deriving Show

defaultOptions :: Options
defaultOptions = Options {
   optHelp = False
 , optTest = False
 , handSize = 10
 , optForceTests = False 
 , optDict = 0
 , optSolve = 0
 , verbocity = 1
 }

options :: [OptDescr (Options -> Options)]
options = 
  [ Option ['h'] ["help"]      (NoArg  (\opts -> opts { optHelp = True })) "Print a help message and exit."
  , Option []    ["test"]      (NoArg  (\opts -> opts { optTest = True })) "Runs a series of tests on your code"
  , Option []    ["handSize"]  (ReqArg (\n opts -> opts { handSize = read n }) "n") "Override the hand size. Defaults to 10."
  , Option [] ["forceTests"]       (NoArg  (\opts -> opts { optTest=True, optForceTests = True})) "Force evaluation of all test cases, regardless of early failures."
  , Option ['q'] ["quiet"] (NoArg (\opts -> opts {verbocity = 0})) "Only print error messages on tests, or minimal output when solving."
  , Option ['d'] ["dict"]     (OptArg (\n opts -> opts { optDict = fromMaybe 1 (fmap read n)}) "n") dictDescr
  , Option ['s'] ["solve"]     (ReqArg (\n opts -> opts { optSolve = readSolve n}) "str") solveDescr
  , Option ['v'] ["verbocity"] (ReqArg (\n opts -> opts { verbocity = read n }) "N") "Set the verbocity of the ooutput. 0-2, 2 being the most verbose."
  ]

showDict :: Int -> String
showDict 0 = "words.txt (86k)"
showDict 1 = "standardDict (4k)"
showDict 2 = "shortDict (1k)"
showDict 3 = "tinyDict (100)"
showDict _ = "tenWords (10)"

getDict :: Int -> IO Dictionary
getDict 0 = longDictIO
getDict 1 = return $ standardDict
getDict 2 = return $ shortDict
getDict 3 = return $ tinyDict
getDict _ = return $ tenWords 
dictDescr = intercalate "\n\t" ["Uses smaller dictionaries for faster execution.",
                                  "Default or -d0\twords.txt (86k)",
                                  "-d or -d1 \tstandardDict (4k)",
                                  "-d2\t\tshortDict (1k)",
                                  "-d3\t\ttinyDict (100)",
                                  "-d4\t\ttenWords (10)"] 
readSolve :: String -> Int
readSolve "greedy" = 1
readSolve "nBrute" = 2
readSolve "sBrute" = 3
readSolve "best" = 4
readSolve "1" = 1
readSolve "2" = 2
readSolve "3" = 3
readSolve "4" = 4
readSolve _ = error "Invalid solver."
solveDescr = intercalate "\n\t" ["Solve the game, instead of playing interactively.",
                                  "greedy\t\tGreedy algorithm.",
                                  "nBrute\t\tNaive brute-force algorithm.",
                                  "sBrute\t\tSmart brute-force algorithm.",
                                  "best\t\tGame tree algorithm."]
-- Return the list of flags
compilerOpts :: [String] -> (Options, [String])
compilerOpts argv =
  case getOpt Permute options argv of
     (o,n,[]) -> (foldl (flip id) defaultOptions o, n)
     (_,_,errs) -> error (concat errs ++ usageInfo header options)
  where header = "Usage: scavenge [OPTION]... [HAND]"

-- Print help
helpIO :: IO()
helpIO = putStrLn $ usageInfo usage options
    where usage = "Usage: ./scavenge [OPTION]... [HAND]"

-- Main IO function
main :: IO()
main = do
  allArgs <- getArgs
  let (opts, args) = compilerOpts allArgs
  when (length args > 1) $ do
         putStrLn "Error: more than one argument. Ignoring all but the first."
  case (optHelp opts, optSolve opts, optTest opts) of
    (True, _, _) -> helpIO
    (_, _, True) -> runTests (verbocity opts) (optForceTests opts)
    (_, k, _) -> do 
        dict <- getDict $ optDict opts
        hand <- getHand args opts
        (handleGame opts k) dict hand

handleGame :: Options -> Int -> Dictionary -> Hand -> IO ()
handleGame opts 0 = playGame
handleGame opts k = 
    solveAndPrint opts k

solveAndPrint :: Options -> Int -> Dictionary -> Hand -> IO ()
solveAndPrint opts k dict hand = do
    let (strat, desc) = case k of
                  1 -> (greedyPlay, "greedy")
                  2 -> (naiveBrutePlay, "naive brute force")
                  3 -> (smartBrutePlay, "smart brute force")
                  4 -> (bestPlay, "game tree")
    if verbocity opts == 0
    then 
      let result = strat dict hand
      in putStrLn $"Play (" ++ (show $ scorePlay result) ++ "): " ++(unwords result) 
    else do
      putStrLn $"Using " ++ desc ++ " strategy with dictionary " ++ (showDict $ optDict opts)
      putStrLn $"Input hand: " ++ hand 
      putStrLn $"Total letter score: " ++ (show $ score hand)
      let result = strat dict hand
      putStrLn $"Play found: " ++ (unwords result)
      putStrLn $"Score: " ++ (show $ scorePlay result)



getHand :: [String] -> Options -> IO String
getHand args opts = 
    if null args 
    then getBetterRandomHand (handSize opts) 
    else return $ map toUpper (head args)
introGame = do
    clearScreen
    setCursorPosition 0 0
    putIMagentaLn "Welcome to Scavenge, the Knapsack-Scrabble game!"
    putMagentaLn "Remember to use \"./scavenge --help\" for help, :s to sort your hand, and :q to quit!\n"
    putCyanLn "You will be dealt a hand of characters."
    putCyanLn "Each character has a numerical value, like in scrabble."
    putCyanLn "Play the combination of moves from your hand that results in the highest score!\n\n"

showGame :: Hand -> Play -> Integer -> IO ()
showGame h p s = do
   setCursorPosition 7 0
   clearFromCursorToScreenEnd
   when (not (null p)) $ do
            putYellow "Words played:\t"
            putWhiteLn $ (concat [x++", "|x<-(init p)]) ++ (last p)
            putYellow "Score:\t\t"
            putWhiteLn $ show s
            return ()
   when (null p) $ do
            putStr "\n\n"
            return ()
   putYellow "Hand:\t\t"
   putWhiteLn h
   putMagentaLn "\n---- Choose a Move----"

endGame :: Hand -> Play -> Integer -> IO ()
endGame h p s = do
    setCursorPosition 11 0
    clearFromCursorToScreenEnd
    putGreenLn "---- Game Over ----"
    when (length p > 0) $ do
            putIYellow "Moves played:\t"
            putIWhiteLn $ (concat [x++", "|x<-(init p)]) ++ (last p)
            return ()
    when (length p == 0) $ do
            putIYellowLn "No moves played!"
    if length h == 0
    then do
            putIGreenLn "CONGRATULATIONS!!! PERFECT GAME!!!"
            putIYellow "Final Score:\t"
            putWhiteLn $ (show s) ++ (" + 50 PERFECT GAME BONUS")
            putWhiteLn $ "\t\t= " ++ (show (s + 50))
    else do
            putIYellow "Remaining Hand:\t"
            putWhiteLn h
            putIYellow "Final Score:\t"
            putWhiteLn $ show s

playGame :: Dictionary -> Hand -> IO ()
playGame dict hand = 
    do introGame
       aux dict hand [] 0
    where aux dict hand play sc = do
             if length hand <= 15 && length (validMoves dict hand) == 0 -- Figures out if they're out of moves
             then endGame hand play sc
             else do
                    showGame hand play sc
                    move <- readMove dict hand play sc
                    case move of
                        ":s" -> aux dict (sort hand) play sc
                        ":q" -> (endGame hand play sc) >> exitSuccess
                        w    -> aux dict (updateHand hand w) (w:play) (score w+sc)
          readMove dict hand play sc = do
            line <- do 
                    putStr ">"
                    getLine
            case line of
                    ":s" -> do
                            return ":s"
                    ":q" -> do
                            return ":q"
                    s@(':':_) -> do
                            putRedLn $ "Command not recognized"
                            readMove dict hand play sc
                    l  -> do
                            let move = map toUpper l
                            if isValidMove dict hand move
                            then return move
                            else do
                                if not $ hand `canMake` move
                                then do
                                    putStrLn $ "Hand cannot make \"" ++ move ++ "\""
                                    readMove dict hand play sc
                                else do
                                    putStrLn $ "\"" ++ move ++ "\" is not in dictionary!"
                                    readMove dict hand play sc


-- Generate a random hand
getRandomHand :: Int -> IO String
getRandomHand n = replicateM n $ randomRIO ('A', 'Z')

getBetterRandomHand :: Int -> IO String
getBetterRandomHand n = do
            hand <- getRandomHand n
            if (fromIntegral (vowels hand)) > ((fromIntegral n) * 0.20)
            then return hand
            else getBetterRandomHand n
            where vowels hand = length [x | x <- hand, x `elem` ['A', 'E', 'I', 'O', 'U']]

