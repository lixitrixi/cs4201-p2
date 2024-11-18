module Tests where

import Defun


-- Function definitions, written as syntax trees

{-
double(val) = val * 2
-}
-- CHANGED NAME: "double" is a reserved word in Java
double_def
    = MkFun "times2" ["val"]
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
              MkPair[x,y] -> x
-}
fst_def
    = MkFun "fst" ["p"]
        (Case (Var "p")
             [IfCon "MkPair" ["x", "y"] (Var "x")])

{-
snd(p) = case p of
              MkPair[x,y] -> y
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
               Cons[y, ys] -> y + sum(ys)
-}
sum_def
    = MkFun "sum" ["xs"]
        (Case (Var "xs")
              [IfCon "Nil" [] (Val 0),
               IfCon "Cons" ["y", "ys"]
                     (BinOp Plus (Var "y") (Call (Var "sum") [Var "ys"]))])

{-
testlist() = Cons[1, Cons[2, Nil]]
-}
testlist_def
    = MkFun "testlist" []
        (Con "Cons" [Val 1, Con "Cons" [Val 2, Con "Nil" []]])

{-
naturals(n) = if n > 0
                then concat(naturals(n-1), Cons[n, Nil])
                else Nil
-}
naturals_def
    = MkFun "naturals" ["n"]
        (If (BinOp Gt (Var "n") (Val 0))
            (Call (Var "concat") [
                Call (Var "naturals") [BinOp Minus (Var "n") (Val 1)],
                Con "Cons" [Var "n", Con "Nil" []]])
            (Con "Nil" []))

{-
map(f, xs) = case xs of
                  Nil -> Nil
                  Cons[y,ys] -> Cons[f(y), map(f,ys)]
-}
map_def
    = MkFun "map" ["f", "xs"]
        (Case (Var "xs")
              [IfCon "Nil" [] (Con "Nil" []),
               IfCon "Cons" ["y", "ys"]
                     (Con "Cons" [Call (Var "f") [Var "y"],
                                  Call (Var "map") [Var "f", Var "ys"]])])

{-
    concat(as, bs) = case as of
                          Nil -> bs
                          Cons[x, xs] -> Cons[x, concat(xs, bs)]
-}
concat_def
    = MkFun "concat" ["as", "bs"]
        (Case (Var "as")
            [IfCon "Nil" [] (Var "bs"),
             IfCon "Cons" ["x", "xs"]
                (Con "Cons" [Var "x", Call (Var "concat") [(Var "xs"), (Var "bs")]])])

allDefs = [double_def, factorial_def, fst_def, snd_def, sum_def,
           testlist_def, map_def, add_def, naturals_def, concat_def]

{-
    times2(3)
-}
-- Should evaluate to 6
testProg1 = MkProg allDefs (Call (Var "times2") [Val 3])

{-
    factorial(5)
-}
-- Should evaluate to 120
testProg2 = MkProg allDefs (Call (Var "factorial") [Val 5])

{-
    fst(MkPair[1, 2])
-}
-- Should evaluate to 1
testProg3 = MkProg allDefs (Call (Var "fst") [Con "MkPair" [Val 1, Val 2]])

{-
    sum(testlist())
-}
-- Should evaluate to 3
testProg4 = MkProg allDefs (Call (Var "sum") [Var "testlist"])

{-
    sum(map times2(), testlist())
-}
-- Should evaluate to 6
testProg5 = MkProg allDefs
                (Call (Var "sum")
                    [Call (Var "map") [Var "times2", Var "testlist"]])

-- Custom test programs

{-
    (add(1))(2)
-}
-- Should evaluate to 3
testProg6 = MkProg allDefs (Call (Call (Var "add") [Val 1]) [Val 2])

{-
    let x = (1 + 2) + (3 + 4)
    x + 5
-}
-- Should evaluate to 15
testProg7 = MkProg [] (Let "x" (BinOp Plus (BinOp Plus (Val 1) (Val 2)) (BinOp Plus (Val 3) (Val 4))) (BinOp Plus (Var "x") (Val 5)))

{-
    concat(naturals(2), naturals(3))
-}
-- Should evaluate to (prettified) [1, 2, 1, 2, 3]
testProg8 = MkProg allDefs (Call (Var "concat") [Call (Var "naturals") [Val 2],
    Call (Var "naturals") [Val 3]])

{-
    let x = 5
    let x = x + 5
    x
-}
-- Should evaluate to 10
testProg9 = MkProg [] (Let "x" (Val 5)
                        (Let "x" (BinOp Plus (Var "x") (Val 5))
                            (Var "x")))

{-
    let con = Fn[times2()] -- holds a partially applied function
    case con of
        Fn[f] -> f(2)
-}
-- Should evaluate to 4
testProg10 = MkProg allDefs (Let "con" (Con "Fn" [Var "times2"])
                                (Case (Var "con") [IfCon "Fn" ["f"] (Call (Var "f") [Val 2])]))

testProg :: String -> Program
testProg "testProg1" = testProg1
testProg "testProg2" = testProg2
testProg "testProg3" = testProg3
testProg "testProg4" = testProg4
testProg "testProg5" = testProg5
testProg "testProg6" = testProg6
testProg "testProg7" = testProg7
testProg "testProg8" = testProg8
testProg "testProg9" = testProg9
testProg "testProg10" = testProg10

testProg _ = error "Specified example program not found; see `src/Tests.hs`"
