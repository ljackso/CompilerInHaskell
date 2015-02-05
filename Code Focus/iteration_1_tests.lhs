
TEST CASES 

If 

> ifTest_1              :: Prog     --Should return Memory (Luke, Integer 21)
> ifTest_1              = If (Val (Integer 1)) (Assign "Luke" (Val (Integer 21)))

> ifTest_2              :: Prog     --Should return empty Memory
> ifTest_2              = If (Val (Integer 0)) (Assign "Luke" (Val (Integer 21)))

> ifTest_3              :: Prog     --Should return Memory (Luke, Integer 21) 
> ifTest_3              = If (Val (Integer 1)) (Seqn [(Assign "Luke" (Val (Integer 21))), (If (Integer 0) (Assign "Jackson" (Val (Integer 1993))))])

If Else 

> ifElseTest_1          :: Prog     --Should return Memory (Luke, Integer 21)
> ifElseTest_1          = IfElse (Val (Integer 1)) (Assign "Luke" (Val (Integer 21))) (Assign "Issy" (Val (Integer 18)))

> ifElseTest_2          :: Prog     --Should return Memory (Issy, Integer 18)
> ifElseTest_2          = IfElse (Val (Integer 0)) (Assign "Luke" (Val (Integer 21))) (Assign "Issy" (Val (Integer 18))) 

Else If

> elseIfTest_1          :: Prog -- m 13
> elseIfTest_1          = ElseIf    (Val (Integer 0)) (Assign "l" (Val (Integer 10))) 
>                                   [   (Case (Val (Integer 0)) (Assign "j" (Val (Integer 11)))),
>                                       (Case (Val (Integer 0)) (Assign "k" (Val (Integer 12))))]
>                                   ((Assign "m" (Val (Integer 13))))

> elseIfTest_2          :: Prog -- k 12
> elseIfTest_2          = ElseIf    (Val (Integer 0)) (Assign "l" (Val (Integer 10))) 
>                                   [   (Case (Val (Integer 0)) (Assign "j" (Val (Integer 11)))),
>                                       (Case (Val (Integer 1)) (Assign "k" (Val (Integer 12))))]
>                                   (Empty)                  


While

> whileTest_1           :: Prog     --Should run for ever
> whileTest_1           = While (Val (Integer 1)) (Assign "Luke" (Val (Integer 21)))

> whileTest_2           :: Prog     --Should do nothing
> whileTest_2           = While (Val (Integer 0)) (Assign "Luke" (Val (Integer 21)))

> whileTest_3           :: Prog     --Should return [(j ,5, (l, 0)] 
> whileTest_3           = Seqn [(Assign "l" (Val (Integer 5))), 
>                               (Assign "j" (Val (Integer 0))), 
>                               (While (Var "l") 
>                                   (Seqn [
>                                       (Assign "j" (ExprApp ADD (Var "j") (Val (Integer 1)))),
>                                       (Assign "l" (ExprApp SUB (Var "l") (Val (Integer 1))))
>                                   ])
>                               )]

 





