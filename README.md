# Scavenge Word Game Project
Creating a word game and using four different AI implementations to solve it

This project description is summarized from the course requirements. The project required implementing the gameplay logic, creating four different solving algorithms, and reducing the complexity of each algorithm for efficiency. Algorithms include a greedy algorithm, two brute force algorithms, and a game tree algorithm.

## Summary
This project required implementing logic for a single-player word game and writing a four simple AIs that solve the game by determining a good (or the best) play. I was not allowed to use higher-order functions, so I depended on list comprehensions and recursion. The rules for the game are as follows:
* The player is dealt a hand of n letters.
* The player makes a word using some of the letters. They are removed from the hand.
* That word is scored as in Scrabble.
* The player then repeats, picking words until either all letters are used or they give up.
* Unused letters are not scored.

## Repository Files
* Scavenge.hs - This is the file where my code was written. I did not modify the other files
* Main.hs - This file contains the I/O actions that turn the code into an executable. 
* Coloring.hs - Contains ANSI code.
* Dictionary.hs - Contains the small dictionaries to determine valid words.
* words.txt - Contains the large (86k words) dictionary.
* Makefile - The makefile supports make and make clean. The executable is output to scavenge.

## Executable
The executable scavenge can be built using `make`. The executable and compiled source files can be removed using `make clean`. By default scavenge will play the game in interactive mode, but it supports the following flags (see -h).

* `-h` or  `--help`	Prints a help message and exits.
* `--test`	Runs unit tests on your code. 
* `--handSize=n`	Sets hand size to n. Default 10.
* `-q` or `--quiet`	Only print error messages on tests, or minimal output when solving.
* `-d[n]` or `--dict[=n]`	Uses smaller dictionaries for faster execution. Values range from 0 (default, 86k words) to 4 (10 words).
* `-s str` or `--solve=str`	Solve the game, instead of playing interactively. str must be greedy, nBrute, sBrute, or best.

## Scavenge Mechanics
For the milestone, you must implement the basic mechanics of Scavenge. You are given the following type aliases to differentiate all the strings you will be handling.

                   type Hand = [Char]
                type Dictionary = [String]
                type Move = String
                type Play = [Move]

We break the mechanics of the game into the following areas: 
Scoring a move (a single string) or a play (a list of moves).
Making a move. This involves updating a hand by removing all the letters of a move from the hand.
Note a letter may occur multiple times in a hand. You should only remove it once for each time that
letter occurs in the move.
Determining if a move/play if valid. For a move of a string to be valid, the string must be a word 
(in a dictionary), and the hand must be able to make the string.
Note that a letter may occur multiple times in a string. For the play to be valid, that letter must occur 
at least that many times in the hand.
Determining all the independently valid moves for a given hand and dictionary. Since the moves are 
independent, using letters from the hand to make one move does not exclude these letters from being 
used in a different way for a different move.

## Solving Scavenge
Each solver takes a dictionary and a hand, and return a play that does well. Only the last solver is guaranteed to return the best possible play.

Greedy algorithm: The greedy algorithm finds a list of all valid moves, and then greedily selects the single best move. After doing so, the hand is updated and the process repeats. Note that the highest-scoring single move might preclude better combinations of words. For example, using the 4k word dictionary and the hand "CLOSEFLOOR", the best possible play is "CLOSE" (7 points) and "FLOOR" (8 points) with a total of 15 points. However, "FORCE" is worth 10 points, and so the greedy algorithm will select that next. After "FORCE" the only word that can be made with the remaining hand LSLOO is "SO", netting a total of 12 points.  
Brute-Force Algorithms: The brute force algorithms are based on the powerset function. The powerset of a set contains every possible subset of that set. For instance, the powerset of {7,1,3} is { {}, {7}, {1}, {3}, {7,1}, {7,3}, {1,3}, {7,1,3} }. The powerset of a set of n elements will have 2n elements. This is the more complicated component of the brute-force algorithms: do not underestimate this problem.

The naive brute-force algorithm computes a list of (almost) every possible play, by taking the powerset of the dictionary. Do not try this with anything but the 10-word dictionary. Each play is then scored, and the maximum play is chosen. Since the powerset of the dictionary will contain every possible combination of words, we must score and find the maximum play.

The smart brute-force algorithm avoids taking the powerset of the entire dictionary. Rather, the smart brute-force algorithm first prunes the dictionary to only the valid moves, and then computes the powerset of the valid moves. This should allow you to solve small hands with reasonable efficiency, even using a very large dictionary. For larger hands, however, the algorithm will be too slow. 

Note that the brute-force algorithms fail when the best play contains a word more than once: since sets can't contain repetitions, the brute force approach misses out on this possibility. There are ways to fix this, but you do not need to consider them.
Recursive Game-Tree algorithm: The recursive game-tree algorithm resembles the greedy algorithm, but chooses the best move more carefully. Instead of just considering the highest-scoring individual move , it computes the best play available for the remaining hand. It then chooses the move with the maximum total score of the move and the resulting best play. This is conceptually the most complicated algorithm, and I highly recommend helper functions.

## Algorithm Complexities
canMake - Linear or  n log(n), instead of quadratic.
validMoves - Linear in the size of the dictionary, assuming a fixed-size hand.
greedyPlay - Low-order polynomial, instead of exponential.
powerset - Exponential, instead of polynomial

## Avoiding Unnecessary Work
Even the game-tree algorithm will be very slow. This is because the branching factor is very large. There are a number of ways to improve the game-tree algorithms by avoiding repetitive work.
Pruning the dictionary: The algorithm goes through the dictionary repetitively. When considering the original hand, it goes through the dictionary to find the valid moves. Then, for each move the algorithm compute the remaining hand and finds the best play for that hand. The first step in finding the best play is, in turn, to compute the valid moves for the remaining hand. However, the remaining hand can't possibly have more valid moves than the original. Thus, the algorithm can safely replace the dictionary with the list of valid moves at each step.
Avoiding permutations of plays: Each play is a list of moves. However, some plays are permutations: they contain the same words in different orders. This means that if the best play is "CLOSE","OF", the game-tree algorithm will consider both "CLOSE","OF" and "OF","CLOSE". This is unnecessary. To avoid this, consider only sorted plays. Different sorting orders might result in different speedups. Note that removing unsorted plays after computing them is probably too late: the algorithm must only generate sorted plays. The best way to do this is left as an exercise for the reader.
Other improvements are possible, but not likely to help much. For instance, modifying the dictionary. Some words are permutations of each-other: they use the same letters in the same combinations, just in different order. These could be removed. On another track, the dictionary can be stored more efficiently: for instance by grouping all the words beginning with a letter, similar to how buildCorpus worked in Project 1. If you change the type of Dictionary, be sure to update buildDict.

## Test Cases
The following test cases should help evaluate the program.
The greedy algorithm should fail to properly solve 
./scavenge -d2 -s greedy CLOSEFLOOR
Giving "FORCE SO" instead of "CLOSE FLOOR" (or another 15-point solution)
However, the smart brute force should be able to solve that case pretty quickly:
./scavenge -d2 -s sBrute CLOSEFLOOR
