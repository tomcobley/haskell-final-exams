module Types where 

import Data.List

type Clue = (String, Int)

type Definition = [String]

type Indicator = [String]

type Link = [String]

data ParseTree = Synonym String |
                 Anagram Indicator String |
                 Reversal Indicator ParseTree |
                 Insertion Indicator ParseTree ParseTree |
                 Charade Indicator ParseTree ParseTree |
                 HiddenWord Indicator String
               deriving (Eq, Ord, Show)

type Parse = (Definition, Link, ParseTree)

type Solution = (Clue, Parse, String)

showParses :: [Parse] -> IO ()
showParses
  = mapM_ showP

showP :: Parse -> IO ()
showP (def, link, tree)
  = do 
      putStrLn ("DEF: " ++ unwords def)
      do if null link
         then return ()
         else putStrLn ("LINK: " ++ unwords link)
      putStr ("TREE: ")
      showT tree

showSolutions :: [Solution] -> IO ()
showSolutions sols
  = do mapM_ showS sols
       putStrLn "----"
  where
    showS ((c, n), p, s)
      = do 
          putStrLn ("\nCLUE: " ++ c ++ " (" ++ show n ++ ")")
          putStrLn ("Solution: " ++ s)
          showP p

showT :: ParseTree -> IO ()
showT t
  = do showT' t
       putStrLn ""
  where
    showT' (Synonym s)
      = putStr ("SYN " ++ s)
    showT' (Anagram ind s)
      = putStr ("ANAG[" ++ unwords ind ++ "] " ++ s)
    showT' (Reversal ind t)
      = do putStr ("REV[" ++ unwords ind ++ "] (")
           showT' t
           putStr ")"
    showT' (Insertion ind t t')
      = do putStr ("INS[" ++ unwords ind ++ "] (")
           showT' t
           putStr ") ("
           showT' t'
           putStr ")"
    showT' (Charade ind t t')
      = do showT' t
           putStr (" +" ++ if null ind then " " else "[" ++ unwords ind ++ "] ")
           showT' t'
    showT' (HiddenWord ind s)
      = do putStr ("HW[" ++ unwords ind ++ "] " ++ s)

