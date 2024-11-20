module Tests where

import Defun


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
              Pair[x,y] -> x
-}
fst_def
    = MkFun "fst" ["p"]
        (Case (Var "p")
             [IfCon "Pair" ["x", "y"] (Var "x")])

{-
snd(p) = case p of
              Pair[x,y] -> y
-}
snd_def
    = MkFun "snd" ["p"]
        (Case (Var "p")
             [IfCon "Pair" ["x", "y"] (Var "y")])

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

{- Extended function definitions -}

true_def = MkFun "true" [] (BinOp Eq (Val 1) (Val 1)) -- Allows clarity in programs
false_def = MkFun "false" [] (BinOp Eq (Val 0) (Val 1))

{-
naturals(n) = if n > 0
                then append(naturals(n-1), Cons[n, Nil])
                else Nil
-}
naturals_def
    = MkFun "naturals" ["n"]
        (If (BinOp Gt (Var "n") (Val 0))
            (Call (Var "append") [
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
    append(as, bs) = case as of
                          Nil -> bs
                          Cons[x, xs] -> Cons[x, append(xs, bs)]
-}
append_def
    = MkFun "append" ["as", "bs"]
        (Case (Var "as")
            [IfCon "Nil" [] (Var "bs"),
             IfCon "Cons" ["x", "xs"]
                (Con "Cons" [Var "x", Call (Var "append") [(Var "xs"), (Var "bs")]])])

{-
    lt(a, b) = a < b
-}
lt_def
    = MkFun "lt" ["a", "b"]
        (BinOp Lt (Var "a") (Var "b"))

{-
    any(xs) case xs of
                Nil -> false
                Cons[x, xs] -> if x
                    then true
                    else any(xs)
-}
any_def
    = MkFun "any" ["xs"]
        (Case (Var "xs") [
            IfCon "Nil" [] (Var "false"),
            IfCon "Cons" ["x", "xs"]
                (If (Var "x")
                    (Var "true")
                    (Call (Var "any") [Var "xs"]))
        ])

{-
    all(xs) case xs of
                Nil -> true
                Cons[x, xs] -> if x
                    then all(xs)
                    else false
-}
all_def
    = MkFun "all" ["xs"]
        (Case (Var "xs") [
            IfCon "Nil" [] (Var "true"),
            IfCon "Cons" ["x", "xs"]
                (If (Var "x")
                    (Call (Var "all") [Var "xs"])
                    (Var "false"))
        ])

bigadd_def
    = MkFun "bigadd" ["a", "b", "c", "d", "e"]
        (BinOp Plus (Var "a") $
            BinOp Plus (Var "b") $
            BinOp Plus (Var "c") $
            BinOp Plus (Var "d") $
            (Var "e"))

fstorsnd_def
    = MkFun "fstorsnd" ["cond"]
        (If (Var "cond")
            (Var "fst")
            (Var "snd"))

length_def
    = MkFun "length" ["list"]
        (Case (Var "list") [
            IfCon "Nil" [] (Val 0),
            IfCon "Cons" ["x", "xs"]
                (BinOp Plus (Val 1) (Call (Var "length") [Var "xs"]))
        ])

allDefs = [double_def, factorial_def, fst_def, snd_def, sum_def,
           testlist_def, map_def, add_def, naturals_def, append_def,
           true_def, false_def, lt_def, any_def, all_def, bigadd_def,
           fstorsnd_def, length_def]

testProg :: Int -> Program

{- Original given test programs -}

{-
    double(3)
-}
-- Should evaluate to 6
testProg 1 = MkProg allDefs (Call (Var "double") [Val 3])

{-
    factorial(5)
-}
-- Should evaluate to 120
testProg 2 = MkProg allDefs (Call (Var "factorial") [Val 5])

{-
    fst(Pair[1, 2])
-}
-- Should evaluate to 1
testProg 3 = MkProg allDefs (Call (Var "fst") [Con "Pair" [Val 1, Val 2]])

{-
    sum(testlist)
-}
-- Should evaluate to 3
testProg 4 = MkProg allDefs (Call (Var "sum") [Var "testlist"])

{-
    sum(map double, testlist)
-}
-- Should evaluate to 6
testProg 5 = MkProg allDefs
                (Call (Var "sum")
                    [Call (Var "map") [Var "double", Var "testlist"]])

-- Extended test programs

{-
    (add(1))(2)
-}
-- Should evaluate to 3
testProg 6 = MkProg allDefs (Call (Call (Var "add") [Val 1]) [Val 2])

{-
    let x = add(3)
    x(2)
-}
-- Should evaluate to 5
testProg 7 = MkProg allDefs (Let "x" (Call (Var "add") [Val 3])
                                (Call (Var "x") [Val 2]))

{-
    let x = (1 + 2) + (3 + 4)
    x + 5
-}
-- Should evaluate to 15
testProg 8 = MkProg [] (Let "x" (BinOp Plus (BinOp Plus (Val 1) (Val 2))
                                (BinOp Plus (Val 3) (Val 4)))
                            (BinOp Plus (Var "x") (Val 5)))

{-
    append(naturals(2), naturals(3))
-}
-- Should evaluate to (prettified) [1, 2, 1, 2, 3]
testProg 9 = MkProg allDefs (Call (Var "append") [Call (Var "naturals") [Val 2],
    Call (Var "naturals") [Val 3]])

{-
    let x = 1
    let x = x + 1
    x
-}
-- Should evaluate to 2
testProg 10 = MkProg [] (Let "x" (Val 1)
                        (Let "x" (BinOp Plus (Var "x") (Val 1))
                            (Var "x")))

{-
    let con = Fn[double()] -- holds a partially applied function
    case con of
        Fn[f] -> f(2)
-}
-- Should evaluate to 4
testProg 11 = MkProg allDefs (Let "con" (Con "Fn" [Var "double"])
                                (Case (Var "con") [IfCon "Fn" ["f"] (Call (Var "f") [Val 2])]))

{-
    let x = (
        let x = 1
        4
    )
    x
-}
-- Should evaluate to 4
testProg 12 = MkProg allDefs (Let "x" (Let "x" (Val 1) (Val 4)) (Var "x"))

{-
    let x = Foo[1, 2]
    case x of
        Foo[x, y] -> x
-}
-- Should evaluate to 1
testProg 13 = MkProg [] (Let "x" (Con "Foo" [Val 1, Val 2])
                            (Case (Var "x") [
                                IfCon "Foo" ["x", "y"] (Var "x") -- Shadow name of constructor
                            ]))

{-
    let x = add()
    x(1, 2)
-}
-- Should evaluate to 3
testProg 14 = MkProg allDefs (Let "x" (Var "add")
                                (Call (Var "x") [Val 1, Val 2]))

-- Write some new tests and expected values for interesting cases (as well as any you think are simply missing) and briefly explain what each is doing.

{-
    any([])
-}
-- Should evaluate to 0 (false)
testProg 15 = MkProg allDefs (Call (Var "any") [Con "Nil" []])

{-
    any([false, false])
-}
-- Should evaluate to 0
testProg 16 = MkProg allDefs (Call (Var "any") [Con "Cons" [Var "false", Con "Cons" [Var "false", Con "Nil" []]]])

{-
    any([false, true])
-}
-- Should evaluate to 1 (true)
testProg 17 = MkProg allDefs (Call (Var "any") [Con "Cons" [Var "false", Con "Cons" [Var "true", Con "Nil" []]]])

{-
    all([])
-}
-- Should evaluate to 1
testProg 18 = MkProg allDefs (Call (Var "all") [Con "Nil" []])

{-
    all([false, true])
-}
-- Should evaluate to 0
testProg 19 = MkProg allDefs (Call (Var "all") [Con "Cons" [Var "false", Con "Cons" [Var "true", Con "Nil" []]]])

{-
    all([true, true])
-}
-- Should evaluate to 1
testProg 20 = MkProg allDefs (Call (Var "all") [Con "Cons" [Var "true", Con "Cons" [Var "true", Con "Nil" []]]])

{-
    any(map(lt(5), naturals(4)))
-}
-- Should evaluate to 0 (false)
testProg 21
    = MkProg allDefs (Call (Var "any")
        [Call (Var "map")
            [Call (Var "lt") [Val 5],
            Call (Var "naturals")
                [Val 4]]])

{-
    any(map(gt(5), naturals(6)))
-}
-- Should evaluate to 1 (true)
testProg 22
    = MkProg allDefs (Call (Var "any")
        [Call (Var "map")
            [Call (Var "lt") [Val 5],
            Call (Var "naturals")
                [Val 6]]])

{-
    let x = (if true
        then 1
        else 2)
    x
-}
-- Should evaluate to 1
testProg 23
    = MkProg allDefs (Let "x" (If (Var "true") (Val 1) (Val 2)) (Var "x"))


{-
    true /\ false
-}
-- Should evaluate to false
testProg 24
    = MkProg allDefs (BinOp And (Var "true") (Var "false"))

{-
    true \/ false
-}
-- Should evaluate to true
testProg 25
    = MkProg allDefs (BinOp Or (Var "true") (Var "false"))

testProg 26
    = MkProg allDefs (Call (Var "bigadd") [
            Call (Var "sum") [Var "testlist"],
            Val 3,
            Call (Var "sum") [Call (Var "naturals") [Val 10]],
            BinOp Times (Val 10) (Val 3),
            If (Var "false") (Val 0) (
                Let "a" (Val 1) (
                    Let "b" (Val 1) (
                        Let "c" (Val 1) (
                            Let "d" (Val 1) (
                                Call (Var "sum") [
                                    Con "Cons" [
                                        Var "a",
                                        Con "Cons" [
                                            Var "b",
                                            Con "Cons" [
                                                Var "c",
                                                Con "Cons" [
                                                    Var "d",
                                                    Con "Nil" []
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            )
                        )
                    )
                )
            )
        ])

testProg 27 -- APPLY will be empty and could cause an "unreachable" error in switch blocks
    = MkProg [testlist_def] (
        Val 1
    )

{-
    fstorsnd(true, Pair[1, 2]) -- overapplication
-}
-- Should evaluate to 1
testProg 28
    = MkProg allDefs (Call (Var "fstorsnd") [
            Var "true",
            Con "Pair" [Val 1, Val 2]
        ])

{-
    length(naturals(100))
-}
-- Should evaluate to 100
testProg 29
    = MkProg allDefs (Call (Var "length") [
        Call (Var "naturals") [Val 100]
    ])

testProg _ = error "Specified example program not found! See `src/Tests.hs`"
