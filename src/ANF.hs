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
     deriving (Show)

data ACaseAlt = AIfCon Name
                       [Name]
                       ANFExpr
     deriving (Show)

data ANFFunction = MkAFun Name
                          [Name]
                          ANFExpr
     deriving (Show)

data ANFProgram = MkAProg { defs :: [ANFFunction],
                            body :: ANFExpr }
     deriving (Show)

-- Convert an input program to ANF, by converting an Expr to an ANFExpr
progToANF :: Program -> ANFProgram
progToANF (MkProg fs m) = MkAProg (map funcToANF fs) (exprToANF 0 m)

funcToANF :: Function -> ANFFunction
funcToANF (MkFun f args body) = MkAFun f args (exprToANF 0 body)

exprToANF :: Int -> Expr -> ANFExpr
exprToANF t (Var x) = AVar x
exprToANF t (Val n) = AVal n
exprToANF t (Let name a b) = ALet name (exprToANF t a) (exprToANF t b)

exprToANF t (If (Var x) a b) = AIf x (exprToANF t a) (exprToANF t b)
exprToANF t (If e a b) =
     let ename = mname t -- We don't mind later variables overwriting this let, as it's immediately consumed
     in ALet ename (exprToANF t e) (AIf ename (exprToANF t a) (exprToANF t b))

exprToANF t (BinOp op a b) =
     let k = (\[aname, bname] -> ABinOp op aname bname)
     in wrapALet t [a, b] k

exprToANF t (Call (Var f) args) =
     let k = ACall f
     in wrapALet t args k
exprToANF _ (Call _ _) = error "Function call not defunctionalised!"

exprToANF t (Con name args) =
     let k = ACon name
     in wrapALet t args k

exprToANF t (Case (Var x) cases) = ACase x (map (caseToANF t) cases)
exprToANF t (Case e cases) =
     let ename = mname t
     in ALet ename (exprToANF t e) (ACase ename (map (caseToANF t) cases))

caseToANF :: Int -> CaseAlt -> ACaseAlt
caseToANF t (IfCon name fields body) = AIfCon name fields (exprToANF t body)

-- Given a list of expressions, create a fresh assignment for each non-variable expression
-- Pass the final list of names to the given continuation
wrapALet :: Int -> [Expr] -> ([Name] -> ANFExpr) -> ANFExpr
wrapALet t [] k             = k []
wrapALet t ((Var x) : es) k = wrapALet t es (\names -> k (x : names))
wrapALet t (e : es) k =
     let ename = mname t  -- generate a fresh variable
     in ALet ename (exprToANF t e) (wrapALet (t + 1) es (\names -> k (ename : names)))

-- Machine name (compiler-defined)
mname :: Int -> Name
mname t = "$t" ++ show t
