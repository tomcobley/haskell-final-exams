module Types where

import Data.Maybe
import Data.List

type Id = String

data Op = Add | Mul | Eq | GEq
        deriving (Eq, Show)

data Exp = Const Int | Var Id | Apply Op Exp Exp 
         deriving (Eq, Show)

data Statement = Assign Id Exp |
                 If Exp Block Block |
                 While Exp Block
               deriving (Eq, Show)

type Block = [Statement]

type Function = (Id, [Id], Block)


type Edge a = (a, a)

type Graph a = ([a], [Edge a])

type IG = Graph Id

type Colour = Int 

type Colouring a = [(a, Colour)]

type IdMap = [(Id, Id)]


type CFG = [((Id, [Id]), [Int])]

------------------------------------------------------------------------
-- Predefined functions...

lookUp :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
lookUp x t
  = fromMaybe (error ("LookUp failed\n  Search key = " ++ show x ++
                      "\n  Table = " ++ show t))
              (lookup x t)

sortGraph :: Ord a => Graph a -> Graph a
sortGraph (ns, es) 
  = (sort ns, sort (map order es))
  where
    order (n, n') = (min n n', max n n')

sortCFG :: CFG -> CFG
sortCFG cfg
  = [((d, sort u), sort ss) | ((d, u), ss) <- cfg]

opNames
  = [(Add, "+"), (Mul, "*"), (Eq, "=="), (GEq, ">=")]

precTable
  = [(Add, 1), (Mul, 2), (Eq, 0), (GEq, 0)]

prec op
  = lookUp op precTable

showArgs []
  = ""
showArgs as
  = foldr1 (\a s -> a ++ (", " ++ s)) as

showExp :: Int -> Exp -> String
showExp _ (Const n)
  = show n
showExp _ (Var id)
  = id
showExp n (Apply op' e e')
  | n > n'    = "(" ++ s ++ ")"
  | otherwise = s
  where
    n' = prec op'
    s = showExp n' e ++ " " ++ fromJust (lookup op' opNames ) ++ " " ++
        showExp n' e'

showLine :: String -> Maybe Int -> Int -> IO ()
showLine s n k
  =  putStrLn (label ++ replicate (k + 3 - length label) ' ' ++ s)
  where label = case n of
          Just n -> show n ++ ": "
          Nothing -> ""

showBlock' b n offset
  = showBlock'' b n offset
  where
    showBlock'' :: Block -> Int -> Int -> IO Int
    showBlock'' [] n k
      = return n
    showBlock'' (s : b) n k
      = do n'  <- showStatement s n k
           n'' <- showBlock'' b n' k
           return n''
    showStatement (Assign id e) n k
      = do showLine (id ++ " = " ++ showExp 0 e ++ ";") (Just n) k
           return (n + 1)
    showStatement (If p q []) n k
      = do showLine ("if " ++ "(" ++ showExp 0 p ++ ") {") (Just n) k
           n' <- showBlock'' q (n + 1) (k + 2)
           showLine "}" Nothing k
           return n'
    showStatement (If p q r) n k
      = do showLine ("if " ++ "(" ++ showExp 0 p ++ ") {") (Just n) k
           n'  <- showBlock'' q (n + 1) (k + 2)
           showLine "} else {" Nothing k
           n'' <- showBlock'' r n' (k + 2)
           showLine "}" Nothing k
           return n''
    showStatement (While p b) n k
      = do showLine ("while " ++ showExp 9 p ++ " {") (Just n) k
           n' <- showBlock'' b (n + 1) (k + 2)
           showLine "}" Nothing k
           return n'

showFun :: Function -> IO()
showFun (name, args, body)
  = do putStrLn ("   fun " ++ name ++ "(" ++ showArgs args ++ ") {")
       n <- showBlock' body 0 2
       showLine "}" Nothing 0

showBlock ::  Block -> IO()
showBlock b
  = do n <- showBlock' b 0 0
       return ()

