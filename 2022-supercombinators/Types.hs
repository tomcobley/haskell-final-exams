module Types where

import Data.List

--
-- Note: A Fun expression can only appear in a Let binding, i.e.
-- there are no anonymous functions. For example,
--
--   App (Fun ["x"] (Var "x")) [Const 3]
-- 
-- is not well formed for the purposes of the exercise.
--

data Exp = Const Int | 
           Var Id | 
           Fun [Id] Exp |
           App Exp [Exp] |
           Let [Binding] Exp 
         deriving (Eq, Show)

type Id = String

type Binding = (Id, Exp)

type Supercombinator = Binding

---------------------------------------------------------
--
-- Some useful functions for debugging...
--

rep = replicate

putExp 
  = putStrLn . showE

showE :: Exp -> String
showE e
  = showE' e ""

showE' :: Exp -> String -> String
showE' (Const c) s 
  = s ++ show c
showE' (Var v) s 
  = s ++ v
showE' (App (Var "ite") [p, q, r]) s 
  = showE' p (s ++ "if ") ++ "\n" ++ rep n ' ' ++ "then\n" ++
    showE' q (rep (n + 2) ' ') ++ "\n" ++ rep n ' ' ++ "else\n" ++
    showE' r (rep (n + 2) ' ')
  where
    n = length s
showE' (Fun as e) s
  = showE' e (s ++ "Fun " ++ unwords as ++ " -> ")
showE' (App f es) s 
  = foldl g (showE' f (s ++ makeStr f "(") ++ makeStr f ")") es 
  where
    makeStr (Var _) _ = ""
    makeStr _ s = s
    g s e@(Const _) = showE' e (s ++ " ")
    g s e@(Var _) = showE' e (s ++ " ")
    g s e = showE' e (s ++ " (") ++ ")"
showE' (Let bs e) s 
  = s ++ "let\n" ++ intercalate "\n" (map g bs) ++
    "\n" ++ showE' e (rep n ' ' ++ "in ")
  where
    g (v, Fun as e) = showF as e (rep (n + 2) ' ' ++ v)
    g (v, d) = showD d (rep (n + 2) ' ' ++ v)
    showF as e s = showE' e (s ++ " " ++ unwords as ++ " = ")
    showD e s = showE' e (s ++ " = ")
    n = length s 



