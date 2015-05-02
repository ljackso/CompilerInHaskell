
TEST CASES 

Take these and copy them across to the relevent file when necessary

TODO: update all test cases to work with return statement

If 

> ifTest_1                      :: Prog     --Should return Memory (Luke, Integer 21)
> ifTest_1                      =  If (Val (Integer 1)) (Assign "Luke" (Val (Integer 21)))

> ifTest_2                      :: Prog     --Should return empty Memory
> ifTest_2                      = If (Val (Integer 0)) (Assign "Luke" (Val (Integer 21)))

> ifTest_3                      :: Prog     --Should return Memory (Luke, Integer 21) 
> ifTest_3                      = If (Val (Integer 1)) (Seqn [(Assign "Luke" (Val (Integer 21))), (If (Integer 0) (Assign "Jackson" (Val (Integer 1993))))])

If Else 

> ifElseTest_1                  :: Prog     --Should return Memory (Luke, Integer 21)
> ifElseTest_1                  = IfElse (Val (Integer 1)) (Assign "Luke" (Val (Integer 21))) (Assign "Issy" (Val (Integer 18)))

> ifElseTest_2                  :: Prog     --Should return Memory (Issy, Integer 18)
> ifElseTest_2                  = IfElse (Val (Integer 0)) (Assign "Luke" (Val (Integer 21))) (Assign "Issy" (Val (Integer 18))) 

Else If

> elseIfTest_1                  :: Prog -- m 13
> elseIfTest_1                  = ElseIf    (Val (Integer 0)) (Assign "l" (Val (Integer 10))) 
>                                           [   (Case (Val (Integer 0)) (Assign "j" (Val (Integer 11)))),
>                                               (Case (Val (Integer 0)) (Assign "k" (Val (Integer 12))))]
>                                          ((Assign "m" (Val (Integer 13))))

> elseIfTest_2                  :: Prog -- k 12
> elseIfTest_2                  = ElseIf    (Val (Integer 0)) (Assign "l" (Val (Integer 10))) 
>                                   [   (Case (Val (Integer 0)) (Assign "j" (Val (Integer 11)))),
>                                       (Case (Val (Integer 1)) (Assign "k" (Val (Integer 12))))]
>                                   (Empty)                  


While

> whileTest_1                   :: Prog     --Should run for ever
> whileTest_1                   = While (Val (Integer 1)) (Assign "Luke" (Val (Integer 21)))

> whileTest_2                   :: Prog     --Should do nothing
> whileTest_2                   = While (Val (Integer 0)) (Assign "Luke" (Val (Integer 21)))

> whileTest_3                   :: Prog     --Should return [(j ,5, (l, 0)] 
> whileTest_3                   = Seqn [(Assign "l" (Val (Integer 5))), 
>                                       (Assign "j" (Val (Integer 0))), 
>                                       (While (Var "l") 
>                                           (Seqn [
>                                               (Assign "j" (ExprApp ADD (Var "j") (Val (Integer 1)))),
>                                               (Assign "l" (ExprApp SUB (Var "l") (Val (Integer 1))))
>                                           ])
>                                       )]

Conditional Tests (using if statements) -- should all return (luke, 21) 

> equalTest_1                   :: Prog 
> equalTest_1                   = If (CompApp EQU (Val (Integer 1)) (Val (Integer 1))) (Assign "Luke" (Val (Integer 21)))

> equalTest_2                   :: Prog
> equalTest_2                   = IfElse (CompApp EQU (Val (Integer 4)) (Val (Integer 1))) (Empty) (Assign "Luke" (Val (Integer 21)))


> notEqualTest_1                :: Prog
> notEqualTest_1                = IfElse (CompApp NEQ (Val (Integer 1)) (Val (Integer 1))) (Empty) (Assign "Luke" (Val (Integer 21)))

> notEqualTest_2                :: Prog
> notEqualTest_2                = IfElse (CompApp NEQ (Val (Integer 21)) (Val (Integer 1))) (Assign "Luke" (Val (Integer 21))) (Empty)

> greaterTest_1                 :: Prog
> greaterTest_1                 = If (CompApp GRT (Val (Integer 4)) (Val (Integer 1))) (Assign "Luke" (Val (Integer 21)))

> greaterTest_2                 :: Prog
> greaterTest_2                 = IfElse (CompApp GRT (Val (Integer 1)) (Val (Integer 21))) (Empty) (Assign "Luke" (Val (Integer 21)))

> lessTest_1                    :: Prog
> lessTest_1                    = If (CompApp LET (Val (Integer 1)) (Val (Integer 4))) (Assign "Luke" (Val (Integer 21)))

> lessTest_2                    :: Prog 
> lessTest_2                    = IfElse (CompApp LET (Val (Integer 21)) (Val (Integer 1))) (Empty) (Assign "Luke" (Val (Integer 21)))

> greaterEqualTest_1            :: Prog
> greaterEqualTest_1            = If (CompApp GEQ (Val (Integer 4)) (Val (Integer 1))) (Assign "Luke" (Val (Integer 21)))

> greaterEqualTest_2            :: Prog
> greaterEqualTest_2            = If (CompApp GEQ (Val (Integer 20)) (Val (Integer 20))) (Assign "Luke" (Val (Integer 21))) 

> greaterEqualTest_3            :: Prog
> greaterEqualTest_3            = IfElse (CompApp GEQ (Val (Integer 1)) (Val (Integer 21))) (Empty) (Assign "Luke" (Val (Integer 21)))

> lessEqualTest_1               :: Prog
> lessEqualTest_1               = If (CompApp LEQ (Val (Integer 1)) (Val (Integer 4))) (Assign "Luke" (Val (Integer 21)))

> lessEqualTest_2               :: Prog
> lessEqualTest_2               = If (CompApp LEQ (Val (Integer 1)) (Val (Integer 1))) (Assign "Luke" (Val (Integer 21)))

> lessEqualTest_3               :: Prog
> lessEqualTest_3               = IfElse (CompApp LEQ (Val (Integer 14)) (Val (Integer 1))) (Empty) (Assign "Luke" (Val (Integer 21)))

Type System test

> doubleTest_1                  :: Prog 
> doubleTest_1                  = Return (ExprApp ADD (Double 2.67) (Double 3.24))

Return tests

> returnTest_1                  :: Prog
> returnTest_1                  = Main (
>                                   Return (Just (ExprApp MUL (ExprApp ADD (Val (Integer 4)) (Val (Integer 4))) (Val (Integer 2))))
>                                   )       

Function Tests

> funcTest_1                    :: Prog
> funcTest_1                    = Seqn [    (Main  
>                                               ( Seqn [ 
>                                                   (Assign "Luke" (FuncCall "testFunc" [(FuncCall "testFunc" [Val (Integer 8), Val (Integer 2)]), Val (Integer 4)])), 
>                                                   (Return (Just (Var "Luke")))]
>                                               )
>                                           ), 
>                                           (Func "testFunc" ["a", "b" ]  
>                                               (Return (Just (ExprApp SUB (Var "a") (Var "b"))))
>                                           )
>                                       ]

Factorial tests, test recursion

> funcTest_2a                   :: Prog
> funcTest_2a                   =   Seqn[   Main 
>                                               ( Seqn[
>                                                   (Assign "fac" (FuncCall "fac_helper" [Val (Integer 12), Val (Integer 1)])),
>                                                   (Return (Just (Var "fac")))
>                                               ]),
>                                           factorial_1,
>                                           factorial_1_Helper
>                                       ]   
>                                   

> factorial_1                   ::Prog
> factorial_1                   = Func "factorial" ["n"] (Return (Just (FuncCall "fac_helper" [Var "n", Val (Integer 1)]))) 
      
> factorial_1_Helper            :: Prog
> factorial_1_Helper            =   Func "fac_helper" ["n", "a"] (
>                                       Seqn [
>                                           IfElse  (CompApp EQU (Var "n") (Val (Integer 0)))
>                                               (Return (Just (Var "a" )))
>                                               (Seqn [
>                                                       Assign "a" (ExprApp MUL (Var "n") (Var "a")),
>                                                       Assign "n" (ExprApp SUB (Var "n") (Val (Integer 1))),
>                                                       (Return (Just (FuncCall "fac_helper" [Var "n", Var "a"])))
>                                               ])
>                                       ]
>                                   )         


> funcTest_2b                   :: Prog
> funcTest_2b                   =   Seqn[   Main 
>                                               ( Seqn[
>                                                   (Assign "fac" (FuncCall "factorial" [Val (Integer 7)])),
>                                                   (Return (Just (Var "fac")))
>                                               ]),
>                                           factorial
>                                       ]   
>                                   




> factorial                     :: Prog
> factorial                     =   Func "factorial" ["n"] (
>                                       Seqn [
>                                           (If (CompApp EQU (Var "n") (Val (Integer 0)))
>                                               (Return (Just (Val (Integer 1))))),
>                                           (Return (Just (ExprApp MUL (Var "n") (FuncCall "factorial" [(ExprApp SUB (Var "n") (Val (Integer 1)))]))))
>                                       ]
>                                   )

Nested function tests

> funcTest_3                    :: Prog
> funcTest_3                    =   Seqn [
>                                       Main (
>                                           Seqn [
>                                               Assign "x" (FuncCall "f" [Val (Integer 3)]),
>                                               Return (Just (Var "x"))
>                                           ]
>                                       ),
>                                       Func "f" ["y"] (
>                                           Return (Just (ExprApp ADD (Val (Integer 1)) (FuncCall "g" [Var "y"])))
>                                       ),
>                                       Func "g" ["z"] (
>                                           Return (Just (ExprApp MUL (Var "z") (Val (Integer 2))))
>                                       )
>                                   ] 

> funcTest_4                    :: Prog
> funcTest_4                    =   Seqn [
>                                       Main (
>                                           Return (Just (ExprApp ADD (Val (Integer 1)) (FuncCall "g" [Val (Integer 3)]) ))
>                                       ),
>                                       Func "g" ["z"] (
>                                           Return (Just (ExprApp MUL (Var "z") (Val (Integer 2))))
>                                       )
>                                   ] 
                               
    





