module Java where

import Defun
import ANF

import Text.Printf
import Data.List (group, sort, intercalate, intersperse)

asgn :: Name -> String -> String
asgn var val = var ++ " = " ++ val ++ ";"

unf :: String -> String
unf val = "new Unf(" ++ val ++ ")"

-- Given a var and an operator it is used in, call the correct value method
operandAs :: Name -> Op -> String
operandAs x op = x ++ castMethod op

tabs :: Int -> String
tabs n = replicate (n * 4) ' '

-- From user 'scvalex' on https://stackoverflow.com/questions/16108714/removing-duplicates-from-a-list-in-haskell-without-elem
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

-- Removes any values in the second list which are elements of the first list
rmOcc :: Eq a => [a] -> [a] -> [a]
rmOcc _ [] = []
rmOcc m (x : xs) = if x `elem` m
     then rmOcc m xs
     else x : rmOcc m xs

-- Turn an expression into a block of formatted Java code
-- Declares and assigns "$ret" to the result of evaluation
-- Does not declare any variables in the given list of names
exprToJava :: [Name] -> ANFExpr -> String
exprToJava def e =
     let (lns, decl) = exprToJava' "$ret" e -- assign expr result to reserved var
         vars = rmOcc def ("$ret" : rmdups decl) -- to declare: unique vars and return var
         lns' = ("Unf " ++ intercalate ", " vars ++ ";") : lns -- declare variables used in body
     in intercalate "\n" $ map (\l -> tabs 2 ++ l) lns'

-- Transform the expression into lines of Java
-- Assigns the given name to the result of the expression
-- Return a list of lines and all declared variables
exprToJava' :: Name -> ANFExpr -> ([String], [Name])
exprToJava' ret (AVar x) = ([asgn ret x], [])
exprToJava' ret (AVal n) = ([asgn ret $ unf (show n)], [])
exprToJava' ret (ABinOp op a b) =
     let ln = asgn ret $ unf $ unwords [operandAs a op, show op, operandAs b op]
     in ([ln], [])
exprToJava' ret (ALet name val b) =
     let (left_lns, left_decl) = exprToJava' name val
         (right_lns, right_decl) = exprToJava' ret b
     in (left_lns ++ right_lns, name : left_decl ++ right_decl)
exprToJava' ret (AIf c a b) =
     let (a_lns, a_decl) = exprToJava' ret a
         (b_lns, b_decl) = exprToJava' ret b
         lns' = ("if (" ++ c ++ ".toBool()) {") : a_lns ++ "} else {" : b_lns ++ ["}"]
     in (lns', a_decl ++ b_decl)
exprToJava' ret (ACall f args) =
     let ln = asgn ret (f ++ "(" ++ intercalate ", " args ++ ")")
     in ([ln], [])
exprToJava' ret (ACon c fields) =
     let c' = "\"" ++ c ++ "\""
         ln = asgn ret ("new Unf(" ++ intercalate ", " (c' : fields) ++ ")")
     in ([ln], [])
exprToJava' ret (ACase c cases) =
     let casesANF = map (caseToJava ret) cases
         lns = concatMap fst casesANF -- body of switch block
         decls = concatMap snd casesANF
         dflt = ["default:",
               "throw new RuntimeException(\"Unmatched constructor type: \" + $con.getTag());"]
         lns' = asgn "$con" c : "switch ($con.getTag()) {" : lns ++ dflt ++ ["}"]
     in (lns', "$con" : decls)

funcToJava :: ANFFunction -> String
funcToJava (MkAFun f args body) =
     let arg_decl = intercalate ", " $ map ("Unf " ++) args -- add type signatures to func args
         hdr = tabs 1 ++ "public static Unf " ++ f ++ "(" ++ arg_decl ++ ") {"
     in intercalate "\n" [hdr, exprToJava args body, tabs 2 ++ "return $ret;", tabs 1 ++ "}"]

caseToJava :: Name -> ACaseAlt -> ([String], [Name])
caseToJava ret (AIfCon name fields body) =
     let (lns, decl) = exprToJava' ret body
         extract (x, i) = asgn x ("$con.getArg(" ++ show i ++ ")")
         matches = zipWith (curry extract) fields [0..]
         lns' = ("case \"" ++ name ++ "\":") : matches ++ lns ++ ["break;"]
     in (lns', decl ++ fields)

-- Compiles the given program into Java with the given template
-- The template has two formattable sections for function definitions and the main program
anfToJava :: String -> ANFProgram -> String
anfToJava template (MkAProg defs body) =
         -- add empty line between functions and flatten into lines
     let fns = intercalate "\n\n" (map funcToJava defs)
         lns = exprToJava [] body ++ "\n" ++ tabs 2 ++ "System.out.println($ret);"
     in printf template fns lns

-- Defunctionalise, convert to ANF, and convert to Java
toJava :: String -> Program -> String
toJava template = anfToJava template . progToANF . defuncProg . renameProg
