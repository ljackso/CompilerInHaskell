
--SMALL SCOPE-- 

> import Data.List

--------------------------------------------------------------------------------

Imperative language:

> data Prog  		=  Assign Name Expr
> 	   		|  If Expr Prog Prog
> 	   		|  While Expr Prog
> 	   		|  Seqn [Prog]
>		           deriving Show
>
> data Expr  		=  Val Int | Var Name | App Op Expr Expr
>			   deriving Show
>
> type Name  		=  Char
>
> data Op    		=  Add | Sub | Mul | Div
>			   deriving Show

Factorial example:

> fac  			:: Int -> Prog
> fac n			=  Seqn [Assign 'A' (Val 1),
>			         Assign 'B' (Val n),
>			         While (Var 'B') (Seqn
>			            [Assign 'A' (App Mul (Var 'A') (Var 'B')),
>			             Assign 'B' (App Sub (Var 'B') (Val (1)))])]

Virtual machine:

> type Stack 		=  [Int]
>
> type Mem 		=  [(Name,Int)]
>
> type Code  		=  [Inst]
> 
> data Inst  		=  PUSH Int
>          		|  PUSHV Name
>          		|  POP Name
>          		|  DO Op
>          		|  JUMP Label
>          		|  JUMPZ Label
>          		|  LABEL Label
>		           deriving Show
> 
> type Label 		=  Int

State monad:

> type State 		=  Label
>
> data ST a 		=  S (State -> (a, State))
>
> apply        		:: ST a -> State -> (a,State)
> apply (S f) x 	=  f x
>
> instance Monad ST where
>    -- return 		:: a -> ST a
>    return x   	=  S (\s -> (x,s))
>
>    -- (>>=)  		:: ST a -> (a -> ST b) -> ST b
>    st >>= f   	=  S (\s -> let (x,s') = apply st s in apply (f x) s')


Fresh function to update states

> fresh :: ST Label
> fresh = S (\s -> (s, s+1))

--------------------------------------------------------------------------------

TEST STUFF

> testCode              :: Code 
> testCode              =   [PUSH 1, POP 'A',
>                           PUSH 8, JUMPZ 0,
>                           LABEL 1,
>                           PUSH 2, PUSH 4,
>                           DO Div, LABEL 0,
>                           JUMPZ 1,
>                           POP 'B'
>                           ] 

> bigTest               :: Code
> bigTest               =   [PUSH 1, POP 'A',
>                           PUSH 10, POP 'B',
>                           LABEL 0,
>                           PUSHV 'B', JUMPZ 1,
>                           PUSHV 'A', PUSHV 'B', DO Mul, POP 'A',
>                           PUSHV 'B', PUSH 1, DO Sub, POP 'B',
>                           JUMP 0,
>                           LABEL 1]

> compTest              :: Prog     -- returns [(L,13)]
> compTest              = If (Val 1) (If  (Val 0) (Assign 'L' (Val 2)) (Assign 'L' (Val 13))) (Assign 'L' (Val 1))
>
> compTest2              :: Prog    -- returns [(A,4)]
> compTest2              = If (Val 1) (Assign 'A' (App Mul (Val 2) (Val 2))) (Assign 'L' (Val 1))
>
> compTest3              :: Prog    -- returns [(A,4),(L,13)] 
> compTest3              = Seqn [(If (Val 1) (Assign 'A' (App Mul (Val 2) (Val 2))) (Assign 'L' (Val 1))), compTest]


--------------------------------------------------------------------------------

COMPILER

Converts program into machine code

> comp :: Prog -> Code
> comp p =  fst (apply (comp' p) (0))


Actually compiles to machine code  

> comp'                 :: Prog -> ST Code
> comp' (Seqn [])       = return []
> comp' (Seqn cs)       = sequenceDealer cs
> comp' (Assign n e)    = return (expression e ++ [POP n])
> comp' (If e p1 p2)    = ifDealer e p1 p2 
> comp' (While e p3)    = whileDealer e p3            


Deals with Expr

> expression            :: Expr -> Code  
> expression (Val i)    = [PUSH i]
> expression (Var v)    = [PUSHV v]
> expression (App o x y)= expression x ++ expression y ++ [DO o] 


Deals with if

> ifDealer              :: Expr -> Prog -> Prog -> ST Code
> ifDealer e p1 p2      =   do  l1 <- fresh
>                               l2 <- fresh
>                               p1code <- comp' p1
>                               p2code <- comp' p2
>                               return (expression e ++ [JUMPZ l1] ++ p1code ++ [JUMP l2] ++ [LABEL l1] ++ p2code ++ [LABEL l2])


Deals with While

> whileDealer           :: Expr -> Prog -> ST Code
> whileDealer e p       =   do  l1 <- fresh
>                               l2 <- fresh
>                               pcode <- comp' p
>                               return ([LABEL l1] ++ expression e ++ [JUMPZ l2] ++ pcode ++ [JUMP l1] ++ [LABEL l2])


Deals With Sequences

> sequenceDealer            :: [Prog] -> ST Code
> sequenceDealer (c:cs)     =   do  head <- comp' c
>                                   tail <- comp' (Seqn cs)
>                                   return (head ++ tail)  


EXECUTER 

This takes a list of machine code instructions and executes them.

> exec                  :: Code -> Mem
> exec c                = exec'  c 0 [] []


This is the function that actually does stuff

> exec'                 :: Code -> Int -> Stack -> Mem -> Mem
> exec' c pc s m        =      
>                               if(length c) <= pc then m
>                               else 
>                                   case c !! pc of 
>                                           PUSH n      -> exec' c (pc+1) (push n s) m
>                                           PUSHV v     -> exec' c (pc+1) (pushv v m s) m
>                                           POP v       -> exec' c (pc+1) (pop s) (assignVariable v (head s) m)
>                                           DO o        -> exec' c (pc+1) (operation o s) m 
>                                           LABEL l     -> exec' c (pc+1) s m
>                                           JUMP l      -> exec' c (jump c l) s m
>                                           JUMPZ l     -> exec' c (jumpz c s l pc) (popz s) m 
>                                                   


STACK FUCNTIONS

This is a series of functions that handle the stack


push onto top of stack an integer

> push                  :: Int -> Stack -> Stack
> push n s              =  [n] ++ s


push variable onto top of the stack

> pushv                 :: Name ->  Mem -> Stack -> Stack
> pushv v m s           =  push (getVariable v m) s


pop an integer from the stack and places it into memory under that variable name

> pop                   :: Stack -> Stack
> pop s                  = tail s


perform an operation on the first two things of the stack

> operation             :: Op -> Stack -> Stack
> operation o s         =   case o of 
>                               Add -> push (v2+v1) ns 
>                               Sub -> push (v2-v1) ns
>                               Mul -> push (v2*v1) ns 
>                               Div -> push (div v2 v1) ns
>                           where
>                               v1 = head s
>                               v2 = head (tail s)
>                               ns = (pop (pop s))    

pops the head if it is 0 if not it leaves it be 

> popz                  :: Stack -> Stack
> popz s                
>                       | top == 0          = pop s
>                       | otherwise         = s
>                       where
>                           top = head s                

MEMORY FUNCTIONS

Functions that deal with memory


This function assigns an integer to a variable

> assignVariable        :: Name -> Int -> Mem -> Mem
> assignVariable  v n m = (deleteVariable v m) ++ [(v, n)]


Get the variable from memory, if not assigned already just assume that variable is 0 

> getVariable           :: Name -> Mem -> Int
> getVariable v ms        
>                       | null memV     = 0 
>                       | otherwise     = snd (head memV)
>                           where 
>                               memV = [ m | m <- ms, fst m == v] 

Checks that the variable isn't being reassigned and if it is deletes that variable

> deleteVariable        :: Name -> Mem -> Mem
> deleteVariable v ms    = [ m | m <- ms, fst m /= v] 


JUMP FUNCTIONS

All the functions that deal with jumping arounbd our code

> jump                  ::  Code -> Int -> Int
> jump cs l             =   case elemIndex True [ isLabel c l | c <- cs] of 
>                               Just n      -> n
>                               Nothing     -> 0     --Should never happen


deals with jumpz

> jumpz                 :: Code -> Stack -> Int -> Int -> Int
> jumpz c s l pc
>                       | head s == 0       = jump c l
>                       | otherwise         = (pc + 1)

returns true if is the label we are looking for

> isLabel               :: Inst -> Int -> Bool
> isLabel c l           =   case c of 
>                               LABEL m         ->  (m == l) 
>                               otherwise       ->  False  
 
--------------------------------------------------------------------------------









  
