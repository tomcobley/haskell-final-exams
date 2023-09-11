module Tests where

import IC.TestSuite
import Data.List

import Types
import Solver
import Examples
import Clues

cleanUpTestCases
  = [ ["half-baked","That's it!","all ok"] ==> 
      ["halfbaked","thats it","all ok"]
    ]

split2TestCases
  = [ [] ==> [],
      [1] ==> [],
      [1,2] ==> [([1],[2])],
      [1,2,3,4] ==> 
        [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]
    ]

split3TestCases
  = [ [] ==> [],
      [1] ==> [],
      [1,2] ==> [([1],[],[2])],
      [1,2,3] ==> 
        [([1],[],[2,3]),([1],[2],[3]),([1,2],[],[3])]
    ]

uninsertTestCases
  = [ [] ==> [],
      [1] ==> [],
      [1,2,3] ==> [([2],[1,3])],
      [1,2,3,4] ==> 
        [([2],[1,3,4]),([2,3],[1,4]),([3],[1,2,4])]
    ]

runMatch k 
  = (fst $ strings !! k, trees !! k) 

matchesTestCases
  = [ runMatch 0 ==> True,
      runMatch 1 ==> True,
      runMatch 4 ==> True,
      runMatch 6 ==> True, 
      runMatch 10 ==> True, 
      runMatch 12 ==> True, 
      runMatch 16 ==> True, 
      runMatch 18 ==> True
    ]

runEvaluate k 
  = (parses !! k, enumerations !! k) 

evaluateTestCases
  = [ runEvaluate 0 ==> ["concern","problem"],
      runEvaluate 1 ==> ["rotates"],
      runEvaluate 4 ==> ["edam"],
      runEvaluate 6 ==> ["amnesty"], 
      runEvaluate 10 ==> ["cabin"], 
      runEvaluate 12 ==> ["fremantle"], 
      runEvaluate 16 ==> ["kensington"], 
      runEvaluate 18 ==> ["speed"]
    ]

parseSynTestCases
  = [ "great" ==> [Synonym "great"],
      "at home" ==> [Synonym "at home"],
      "triangular mollusc" ==> []
    ]

parseAnagTestCases
  = [ "mixed bag" ==> [Anagram ["mixed"] "bag"],
      "bag mixed" ==> [Anagram ["mixed"] "bag"],
      "changed the car" ==> [Anagram ["changed"] "thecar"],
      "fruit cake" ==> []
    ]

parseRevTestCases
  = [ "go backwards" ==> [Reversal ["backwards"] (Synonym "go")],
      "backwards go" ==> [Reversal ["backwards"] (Synonym "go")],
      "mixed bag" ==> []
    ]

parseInsTestCases
  = [ "back in business" ==> 
        [Insertion ["in"] (Synonym "back") (Synonym "business")],
      "work around town" ==> 
        [Insertion ["around"] (Synonym "town") (Synonym "work")],
     "back pain" ==> []
    ]

parseCharadeTestCases
  = [ "stop go" ==> [Charade [] (Synonym "stop") (Synonym "go")],
      "stop and go" ==> 
        [Charade [] (Synonym "stop") (Charade [] (Synonym "and") (Synonym "go")),
         Charade [] (Charade [] (Synonym "stop") (Synonym "and")) (Synonym "go"),
         Charade ["and"] (Synonym "stop") (Synonym "go")],
      "go after stop" ==> 
        [Charade [] (Synonym "go") (Charade [] (Synonym "after") (Synonym "stop")),
         Charade [] (Charade [] (Synonym "go") (Synonym "after")) (Synonym "stop"),
         Charade ["after"] (Synonym "stop") (Synonym "go")]
    ]

runExtra f s
  = f (words s)

parseExtraParseTestCases
  = [ runExtra parseReversal "back at home" ==> 
        [Reversal ["back"] (Synonym "at home"),
         Reversal ["back"] (Charade [] (Synonym "at") (Synonym "home"))],
      runExtra parseInsertion "was back in time" ==> 
        [Insertion ["in"] (Reversal ["back"] (Synonym "was")) (Synonym "time"),
        Insertion ["in"] (Charade [] (Synonym "was") (Synonym "back")) (Synonym "time")]
    ]

parseClueTestCases
  = [ 12 ==> 425,
      13 ==> 412,
      14 ==> 38,
      15 ==> 24,
      16 ==> 72,
      17 ==> 66,
      18 ==> 13
    ]

solveTestCases
  = [ solveAll 19 ==> 
       [["concern","problem"],["rotates"],["redshank"],["master"],
        ["edam"],["repaid"],["amnesty"],["remainder"],["sustain"],
        ["loire"],["cabin"],["snappy"],["fremantle"],["nasty"],
        ["rotate"],["inapt"],["kensington"],["defiant"],["speed"]] 
    ]

allTestCases
  = [
      TestCase  "cleanUp" (map cleanUp)
                cleanUpTestCases
      ,
      TestCase  "split2" (sort . split2)
                split2TestCases
      ,
      TestCase  "split3" (sort . split3)
                split3TestCases
      ,
      TestCase  "uninsert" (sort . uninsert)
                uninsertTestCases
      ,
      TestCase  "matches" (uncurry matches)
                matchesTestCases
      ,
      TestCase  "evaluate" (sort . uncurry evaluate)
                evaluateTestCases
      ,
      TestCase  "parseSynonym" (sort . parseSynonym . words)
                parseSynTestCases
      ,
      TestCase  "parseAnagram" (sort . parseAnagram . words)
                parseAnagTestCases
      ,
      TestCase  "parseReversal" (sort . parseReversal . words)
                parseRevTestCases
      ,
      TestCase  "parseInsertion" (sort . parseInsertion . words)
                parseInsTestCases
      ,
      TestCase  "parseCharade" (sort . parseCharade . words)
                parseCharadeTestCases
      ,
      TestCase  "extraParseCases" sort
                parseExtraParseTestCases
      ,
      TestCase  "parseClueLengths" (length . parseClue . (clues !!))
                parseClueTestCases
      ,
      TestCase  "solveAll" (map sort) 
                solveTestCases
    ]

runTests = mapM_ goTest allTestCases


main = runTests
