module ANF where

import Defun

data ANFExpr
     = AVar Name
     | AVal Int
     | ALet Name ANFExpr ANFExpr
     | ACall Name [Name]
     | ACon Name [Name]
     | AIf Name ANFExpr ANFExpr
     | ABinOp Op Name Name
     | ACase Name [ACaseAlt]

data ACaseAlt = AIfCon Name [Name] ANFExpr

data ANFFunction = MkAFun { fnName :: Name,
                            argNames :: [Name],
                            definition :: ANFExpr }

data ANFProgram = MkProg { defs :: [ANFFunction],
                           mainExpr :: ANFExpr }

-- Convert an input program to ANF, by converting an Expr to an ANFExpr
-- Note that you will need another intermedaite step, converting an Expr
-- to a defunctionalised Expr (that is, an Expr where every function is
-- applied to exactly the right number of arguments)
progToANF :: Program -> ANFProgram
progToANF = undefined

anfToJava :: ANFProgram -> String
anfToJava = undefined

toJava :: Program -> String
toJava p = anfToJava (progToANF p)

-- All being well, this will print out a java program which, when compiled,
-- will print the result of evaluating the expression in testProg1
main :: IO ()
main = putStrLn (toJava testProg1)
