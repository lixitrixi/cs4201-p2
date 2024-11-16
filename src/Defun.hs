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
-- instance Show Expr where
--     show (Var x) = x
--     show (Val n) = show n
--     show (Let var val b) = "let " ++ var ++ " = (" ++ show val ++ ") in " ++ show b
--     show (Call f )

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

-- Defunctionalise an expression TODO
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

-- For each function TODO
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

-- TODO
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

-- Function definitions, written as syntax trees

{-
double(val) = val * 2
-}
double_def
    = MkFun "double" ["val"]
        (BinOp Times (Var "val") (Val 2))

{-
factorial(x) = if x == 0
                  then 1
                  else x * factorial(x-1)
-}
factorial_def
    = MkFun "factorial" ["x"]
        (If (BinOp Eq (Var "x") (Val 0))
            (Val 1)
            (BinOp Times
                   (Var "x")
                   (Call (Var "factorial") [BinOp Minus (Var "x") (Val 1)])))

{-
fst(p) = case p of
              MkPair(x,y) -> x
-}
fst_def
    = MkFun "fst" ["p"]
        (Case (Var "p")
             [IfCon "MkPair" ["x", "y"] (Var "x")])

{-
snd(p) = case p of
              MkPair(x,y) -> y
-}
snd_def
    = MkFun "snd" ["p"]
        (Case (Var "p")
             [IfCon "MkPair" ["x", "y"] (Var "y")])

{-
add(x, y) = x + y
-}
add_def
    = MkFun "add" ["x", "y"]
        (BinOp Plus (Var "x") (Var "y"))

{-
sum(xs) = case xs of
               Nil -> 0
               Cons(y, ys) -> y + sum(ys)
-}
sum_def
    = MkFun "sum" ["xs"]
        (Case (Var "xs")
              [IfCon "Nil" [] (Val 0),
               IfCon "Cons" ["y", "ys"]
                     (BinOp Plus (Var "y") (Call (Var "sum") [Var "ys"]))])

{-
testlist() = Cons 1 (Cons 2 Nil)
-}
testlist_def
    = MkFun "testlist" []
        (Con "Cons" [Val 1, Con "Cons" [Val 2, Con "Nil" []]])

{-
map(f, xs) = case xs of
                  Nil -> Nil
                  Cons(y,ys) -> Cons(f(y), map(f,ys))
-}
map_def
    = MkFun "map" ["f", "xs"]
        (Case (Var "xs")
              [IfCon "Nil" [] (Con "Nil" []),
               IfCon "Cons" ["y", "ys"]
                     (Con "Cons" [Call (Var "f") [Var "y"],
                                  Call (Var "map") [Var "f", Var "ys"]])])

allDefs = [double_def, factorial_def, fst_def, snd_def, sum_def,
           testlist_def, map_def, add_def]


-- Should evaluate to 6
testProg1 = MkProg allDefs (Call (Var "double") [Val 3])

-- Should evaluate to 120
testProg2 = MkProg allDefs (Call (Var "factorial") [Val 5])

-- Should evaluate to 1
testProg3 = MkProg allDefs (Call (Var "fst") [Con "MkPair" [Val 1, Val 2]])

-- Should evaluate to 3
testProg4 = MkProg allDefs (Call (Var "sum") [Var "testlist"])
                      
-- Should evaluate to 6
testProg5 = MkProg allDefs
              (Call (Var "sum")
                [Call (Var "map") [Var "double", Var "testlist"]])

-- Custom test programs

-- Should evaluate to 3
testProg6 = MkProg allDefs (Call (Call (Var "add") [Val 1]) [Val 2])

testProg7 = MkProg [] (Let "x" (BinOp Plus (Val 1) (BinOp Plus (Val 2) (Val 3))) (Var "x"))

testProg8 = MkProg [] (Let "x" (BinOp Plus (BinOp Plus (Val 1) (Val 2)) (BinOp Plus (Val 3) (Val 4))) (BinOp Plus (Var "x") (Val 5)))
