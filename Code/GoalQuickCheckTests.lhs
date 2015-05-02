
Testing Modules 

> import Test.QuickCheck

My Modules

> import GoParser 
> import GoRunner 

USed to generate random values;

> instance Arbitrary ArthOp where
>   arbitrary = oneof
>                   [return ADD, return SUB, return DIV, return MUL]

> instance Arbitrary CompOp where
>   arbitrary = oneof
>                   [return EQU, return NEQ, return LEQ, return GEQ, return GRT, return LET]
    
> instance Arbitrary Number where
>   arbitrary = do x <- arbitrary
>                  return (Integer x)


Testing Arthemtic Operations on Integers

> propAdd l j                   = ((addNumbers (Integer l) (Integer j)) == Integer (l + j))
> propSub l j                   = ((subNumbers (Integer l) (Integer j)) == Integer (l - j))
> propMul l j                   = ((mulNumbers (Integer l) (Integer j)) == Integer (l * j))
> propDiv l j                   = ((divNumbers (Integer l) (Integer j)) == Integer (quot l j))

Testing Comparison Operations on Integers 

> propEqu l j                   = ((isNumEqu (Integer l) (Integer j)) == (l == j)) 
> propNeq l j                   = ((isNumNeq (Integer l) (Integer j)) == (l /= j)) 
> propGeq l j                   = ((isNumGeq (Integer l) (Integer j)) == (l >= j))
> propLeq l j                   = ((isNumLeq (Integer l) (Integer j)) == (l <= j))  
> propGrt l j                   = ((isNumGrt (Integer l) (Integer j)) == (l > j))
> propLet l j                   = ((isNumLet (Integer l) (Integer j)) == (l < j)) 


Test performing an arthemetic operation on the stack
TODO: Check if you can generate of lists a certain size 

> propStackArthOp  o xs         = case o of 
>                                   ADD     -> propStackAdd xs
>                                   SUB     -> propStackSub xs
>                                   MUL     -> propStackMul xs 
>                                   DIV     -> propStackDiv xs 

> propStackAdd (x:y:xs)         = ((operation ADD (x:y:xs)) == [addNumbers y x] ++ xs)
> propStackAdd _                = True      -- As this case will never happen so it's okay  

> propStackSub (x:y:xs)         = ((operation SUB (x:y:xs)) == [subNumbers y x] ++ xs)
> propStackSub _                = True      -- As this case will never happen so it's okay 

> propStackMul (x:y:xs)         = ((operation MUL (x:y:xs)) == [mulNumbers y x] ++ xs)
> propStackMul _                = True      -- As this case will never happen so it's okay 

> propStackDiv (x:y:xs)         = ((operation DIV (x:y:xs)) == [divNumbers y x] ++ xs)
> propStackDiv _                = True      -- As this case will never happen so it's okay 


Tests Performing comparisons on the stack. 

> propStackCompOp o xs          = case o of
>                                   EQU     -> propStackEqu xs
>                                   NEQ     -> propStackNeq xs
>                                   GEQ     -> propStackGeq xs
>                                   LEQ     -> propStackLeq xs
>                                   GRT     -> propStackGrt xs
>                                   LET     -> propStackLet xs

> numBool                       :: Bool -> Number 
> numBool True                  = (Integer 1)
> numBool False                 = (Integer 0)  

> propStackEqu (x:y:xs)         = ((comparison EQU (x:y:xs)) == [numBool(isNumEqu y x)] ++ xs)
> propStackEqu xs               = (length xs < 2)   -- ensures that if it hits this is because of list size

> propStackNeq (x:y:xs)         = ((comparison NEQ (x:y:xs)) == [numBool(isNumNeq y x)] ++ xs)
> propStackNeq xs               = (length xs < 2)   -- ensures that if it hits this is because of list size

> propStackGeq (x:y:xs)         = ((comparison GEQ (x:y:xs)) == [numBool(isNumGeq y x)] ++ xs)
> propStackGeq xs               = (length xs < 2)   -- ensures that if it hits this is because of list size

> propStackLeq (x:y:xs)         = ((comparison LEQ (x:y:xs)) == [numBool(isNumLeq y x)] ++ xs)
> propStackLeq xs               = (length xs < 2)   -- ensures that if it hits this is because of list size

> propStackGrt (x:y:xs)         = ((comparison GRT (x:y:xs)) == [numBool(isNumGrt y x)] ++ xs)
> propStackGrt xs               = (length xs < 2)   -- ensures that if it hits this is because of list size

> propStackLet (x:y:xs)         = ((comparison LET (x:y:xs)) == [numBool(isNumLet y x)] ++ xs)
> propStackLet xs               = (length xs < 2)   -- ensures that if it hits this is because of list size



