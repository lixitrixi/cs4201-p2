module Defun where

import Data.List

type Name = String

data Op = Plus | Minus | Times | Divide | Eq | Lt | Gt | And | Or
instance Show Op where
    show Plus = "+"
    show Minus = "-"
    show Times = "*"
    show Divide = "/"
    show Eq = "=="
    show Lt = "<"
    show Gt = ">"
    show And = "&&"
    show Or = "||"

-- "i" for ops that expect integers, "b" for booleans
opType :: Op -> String
opType And = "b"
opType Or = "b"
opType _ = "i"

data Expr
     = Var Name
     | Val Int
     | Let Name Expr Expr  -- let x = 94 in double(x) + x
     | Call Expr [Expr]    -- double(2)
     | Con Name [Expr]     -- Nil() Cons(x, xs)
     | If Expr Expr Expr   -- if x == y then 1 else 2
     | BinOp Op Expr Expr  -- x == y (only integer operations)
     | Case Expr [CaseAlt] -- case exp of { Nil() -> empty(); Cons(x, xs) -> nonempty(x, xs) }
    deriving (Show)

data CaseAlt = IfCon Name -- constructor name
                     [Name] -- field names
                     Expr -- can access fields using names above
    deriving (Show)

data Function = MkFun Name -- function name
                      [Name] -- argument names
                      Expr -- function body
    deriving (Show)

data Program = MkProg [Function] -- all the function definitions
                      Expr -- expression to evaluate
    deriving (Show)

-- Defunctionalisation helper functions

-- Defunctionalise a program by converting it to an equivalent form,
-- where every function is applied to exactly the right number of arguments
defuncProg :: Program -> Program
defuncProg (MkProg funcs body) =
     let applyFun = MkFun
          "__APPLY" -- Compiler-reserved names begin with "__"
          ["f", "arg"]
          (Case (Var "f") (genApplyAlt funcs))
     in MkProg (applyFun : map (defuncFunc funcs) funcs) (defuncExpr funcs body)

-- Defunctionalise an expression given a list of declared functions
defuncExpr :: [Function] -> Expr -> Expr
defuncExpr fs (Var x) = Var x
defuncExpr fs (Val n) = Val n
defuncExpr fs (Let name a b) = Let name (defuncExpr fs a) (defuncExpr fs b)
defuncExpr fs (Con name args) = Con name (map (defuncExpr fs) args)
defuncExpr fs (If c a b) = If (defuncExpr fs c) (defuncExpr fs a) (defuncExpr fs b)
defuncExpr fs (BinOp op a b) = BinOp op (defuncExpr fs a) (defuncExpr fs b)
defuncExpr fs (Case a cs) = Case (defuncExpr fs a) (map (defuncCase fs) cs)

defuncExpr fs (Call (Var name) args) = 
     let decl = find (\(MkFun f _ _) -> f == name) fs
     in case decl of
          Nothing -> wrapApply (Var name) args -- local name
          Just (MkFun _ dargs _) -> let arity = length dargs
               in if length args < arity
                    then Con (defuncConName name (length args)) args -- partial application
                    else wrapApply (Call (Var name) (take arity args)) (drop arity args) -- over-application
defuncExpr fs (Call e args) = wrapApply (defuncExpr fs e) args -- we don't know the returned function

-- Since case statements define a local scope we ignore any bound function names
defuncCase :: [Function] -> CaseAlt -> CaseAlt
defuncCase fs (IfCon name fields a) = IfCon name fields
     (defuncExpr (rmFuncs fs fields) a)

defuncFunc :: [Function] -> Function -> Function
defuncFunc fs (MkFun name args body) = MkFun name args (defuncExpr (rmFuncs fs args) body)

-- For each function, create a case for all its partial applications
genApplyAlt :: [Function] -> [CaseAlt]
genApplyAlt []       = []
genApplyAlt (f : fs) =
     let MkFun name args _ = f
         arity = length args
         alts = map (defuncCaseAlt name arity) (take arity [0..])
     in alts ++ genApplyAlt fs

wrapApply :: Expr -> [Expr] -> Expr
wrapApply = foldl (\e x -> Call (Var "__APPLY") [e, x])

-- Removes functions of the same name; used to remove local variables
rmFunc :: [Function] -> Name -> [Function]
rmFunc fs name = filter (\(MkFun f _ _) -> f /= name) fs

-- Removes functions with any of the given names
rmFuncs :: [Function] -> [Name] -> [Function]
rmFuncs = foldr (flip rmFunc)

-- For a partial application to the given number of arguments,
--   apply the function if we've reached its arity
--   otherwise return the next largest partial application
defuncCaseAlt :: Name -> Int -> Int -> CaseAlt
defuncCaseAlt name ar n =
     let fields = map (\i -> "a" ++ show i) (take n [0..])
     in IfCon
          (defuncConName name n)
          fields
          (if n < ar - 1
               then Con
                    (defuncConName name (n + 1))
                    (map Var fields ++ [Var "arg"])
               else Call
                    (Var name)
                    (map Var fields ++ [Var "arg"]))

-- Given a function name and number of arguments given to it,
--   generate the corresponding function construct name
defuncConName :: Name -> Int -> Name
defuncConName name n = "__FN_" ++ show n ++ "_" ++ name
