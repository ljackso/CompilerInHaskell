
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

While

> WhileTest_1           :: Prog     --Should run for ever
> WhileTest_1           = While (Val (Integer 1)) (Assign "Luke" (Val (Integer 21)))

> WhileTest_2           :: Prog     --Should do nothing
> WhileTest_2           = While (Val (Integer 0)) (Assign "Luke" (Val (Integer 21)))

> WhileTest_3           :: Prog 
> WhileTest_3           = Seqn [(Assign "l" (Val (Integer 5)), (Assign "j" (Val (Integer 0))), (While (Var "l") Seqn [ 