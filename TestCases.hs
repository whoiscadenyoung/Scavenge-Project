import Dictionaries
import Scavenge
import Data.List ((\\))
handOne = "ABCDEFG"
handTwo = "AAABBBCCC"

handFour = "TESTHAND"
handFive = "DNAHTSET"

--isValidMove tests
vmHandOne = "MKEKIOCHAOX"
vmHandTwo = "MKKIOCHAOX"
vmGoodMove = "MAKE"
vmBadMove = "MAKI"

--isValidPlay tests
vpHand = "THEQUICKBROWN"
vpPlay1 = ["THE", "QUICK", "BROWN"]
vpPlay2 = ["THE", "THE", "QUICK", "QUICK", "BROWN", "BROWN"]
vpPlay3 = ["THEQUICKBROWN"]
        
--Dictionaries for solvers
bruteDict = ["CLOSE", "FLOOR", "OTHER", "YOUR", "FORCE", "SO"]
tinyWithoutIt = tinyDict \\ ["IT"]
bestPlayDict = ["CELL","CLOSE","FLOOR","FOR","FORCE", "THING", "THINK", "THIS",
                         "THOSE", "TIME", "TO", "TWO", "UP", "USE", "VERY", "WANT", "WAY", "WE", "WELL",
                         "WHAT", "WHEN", "WHICH", "WHO", "WILL", "WITH", "WOULD", "YEAR", "YOU", "YOUR"] 


