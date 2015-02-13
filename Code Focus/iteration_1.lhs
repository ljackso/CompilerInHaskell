
> import Data.List

ITERATION ONE

This is a new iteration of my project where I focus on code genration, for now igonring the 
problems parsing was giving. I treat my input as a high level representation of Go as a data 
structure

-----------------------------------------------------------------------------------------------------

MONANDS

Define any monads needed here

State monad:

> type State 		            =  Label
>
> data ST a 		            =  S (State -> (a, State))
>
> apply        		            :: ST a -> State -> (a,State)
> apply (S f) x 	            =  f x
>
> instance Monad ST where
>    --return 		            :: a -> ST a
>    return x   	            =  S (\s -> (x,s))
>
>    --(>>=)  		            :: ST a -> (a -> ST b) -> ST b
>    st >>= f   	            = S (\s -> let (x,s') = apply st s in apply (f x) s')


Fresh function to update states

> fresh                         :: ST Label
> fresh                         = S (\s -> (s, s+1))

-----------------------------------------------------------------------------------------------------

HIGH LEVEL

First define the program features we want to compile these are amore formal definitons of 
Go's features,

It is what we want our Program to be represented by once we have manged to parse it.

> data Prog                     = Assign Name Expr
> 	   		                        | If Expr Prog
>                                   | IfElse Expr Prog Prog
>                                   | ElseIf Expr Prog [ElseIfCase'] Prog       
> 	   		                        | While Expr Prog           
> 	   		                        | Seqn [Prog]
>                                   | Empty                     -- Allows blank Prog may be useful 
>                                   | Return (Expr)             -- Change so can Return nothing 
>                                   | Func Name Prog
>                                   | FuncCall Name [Expr]      -- list of Expr is the arguments (can be empty)
>		                                deriving Show
>
> data ElseIfCase'              = Case Expr Prog     --used for switch cases and else if 
>                                   deriving Show

How variables are represented and used.

> data Expr  		            =  Val Number | Var Name | ExprApp ArthOp Expr Expr | CompApp CompOp Expr Expr 
>			                        deriving Show
>
> data ArthOp    		        =  ADD | SUB | MUL | DIV        -- Arithmetic operations
>			                        deriving Show
>
> type Name  		            =  String

How conditionals are expressed

> data CompOp                   = EQU | NEQ | GEQ | LEQ | GRT | LET    -- | AND | OR      -- Conditional operations
>			                        deriving Show

Type System

For now just defing a generic type of just ints that can be updated

> data Number                   = Integer Int | Double Double
>			                        deriving (Show, Eq)

-----------------------------------------------------------------------------------------------------

TEST CASES 

> returnTest_1                  :: Prog
> returnTest_1                  = Seqn [(Assign "Luke" (Val (Integer 4))), (Return (Var "Luke"))]


-----------------------------------------------------------------------------------------------------

COMPILER

Converts program into machine code

> comp                          :: Prog -> Code
> comp p                        =  fst (apply (comp' p) (0))


Actually compiles to machine code  

> comp'                         :: Prog -> ST Code
> comp' (Func n p1)             = functionDealer n p1 
> comp' (Seqn cs)               = sequenceDealer cs
> comp' (Assign n e)            = return (expression e ++ [POP n])
> comp' (If c p)				= (ifDealer c p)
> comp' (IfElse c p1 p2)        = (ifElseDealer c p1 p2)
> comp' (ElseIf c p1 cs p2)     = (elseIfDealer c p1 cs p2)
> comp' (While e p3)            = (whileDealer e p3)
> comp' (Empty)                 = return []  
> comp' (Return e)              = (returnDealer e)          


Deals with Expr

> expression                    :: Expr -> Code
> expression (Val i)            = [PUSH i]
> expression (Var v)            = [PUSHV v]
> expression (ExprApp o x y)    = expression x ++ expression y ++ [DO o] 
> expression (CompApp o x y)	= expression x ++ expression y ++ [COMP o] 

Deals with if 

> ifDealer                      :: Expr -> Prog -> ST Code 
> ifDealer c p1                 =   do  l       <- fresh
>                                       p1code  <- comp' p1
>                                       return (expression c ++ [JUMPZ l] ++ p1code ++ [LABEL l])   

Deals with if else

> ifElseDealer                  :: Expr -> Prog -> Prog -> ST Code
> ifElseDealer c p1 p2          =   do  l1 		<- fresh
>                                       l2      <- fresh
>                                       p1code 	<- comp' p1
>                                       p2code 	<- comp' p2
>                                       return (expression c ++ [JUMPZ l1] ++ p1code ++ [JUMP l2] ++ [LABEL l1] ++ p2code ++ [LABEL l2])

Deals with else if 

> elseIfDealer                  :: Expr -> Prog -> [ElseIfCase'] -> Prog -> ST Code 
> elseIfDealer  c p1 cs p2      =   do  l1      <- fresh
>                                       l2      <- fresh
>                                       el      <- fresh
>                                       p1code  <- comp' p1
>                                       p2code  <- comp' p2
>                                       csCode  <- listCaseDealer cs el      
>                                       return (expression c ++ [JUMPZ l1] ++ p1code ++ [JUMP el] ++ [LABEL l1] ++ csCode ++ p2code ++ [LABEL el])

TODO: Possibly exapnd these functions to take in switch cases

Deals with a list of else if cases

> listCaseDealer                :: [ElseIfCase'] -> Label -> ST Code
> listCaseDealer [] el          = return []  
> listCaseDealer (c:cs) el      =   do  cCode   <- (caseDealer c el)
>                                       csCode  <- (listCaseDealer cs el) 
>                                       return (cCode ++ csCode)

Deals with an individual else if case

> caseDealer                    :: ElseIfCase' -> Label -> ST Code
> caseDealer (Case c p1) el     =   do  l 		<- fresh
>                                       p1code 	<- comp' p1
>                                       return (expression c ++ [JUMPZ l] ++ p1code ++ [JUMP el] ++ [LABEL l])                          


Deals with While    

> whileDealer                   :: Expr -> Prog -> ST Code
> whileDealer e p               =   do  l1      <- fresh
>                                       l2      <- fresh
>                                       pcode   <- comp' p
>                                       return ([LABEL l1] ++ expression e ++ [JUMPZ l2] ++ pcode ++ [JUMP l1] ++ [LABEL l2])

Deals with Return 

> returnDealer					:: Expr -> ST Code
> returnDealer e				=  return (expression e ++ [STOP])

Deals With Sequences

> sequenceDealer                :: [Prog] -> ST Code 
> sequenceDealer []             = return []
> sequenceDealer (c:cs)         =   do  head    <- comp' c
>                                       tail    <- comp' (Seqn cs)
>                                       return (head ++ tail)  

Deals with Functions, assumes that ever function contains a return will be done at parsing level

> functionDealer                :: Name -> Prog -> ST Code
> functionDealer n p1           =   do  p1code  <- comp' p1 
>                                       return ([FUNC n] ++ p1code ++ [FEND]) 


-----------------------------------------------------------------------------------------------------

LOW LEVEL

Now we define what we ant our high level code to compile down too, for now I define my own low
level code and create and executer for it. 

This section can either pre repleaced or used as an intermediatary for a more useful low level 
language.

This can be represented as a mini virtual machine, we will use the same vm as was used in AFP so
as to focus n high level interpritation, look to improve this later. 

> type Stack 		            =  [Number]
>
> type Mem 		                =  [(Name, Number)]		
>   
> type Code  		            =  [Inst]
> 
> data Inst  		            =  PUSH Number
>          		                    | PUSHV Name
>          		                    | POP Name
>          		                    | DO ArthOp
>          		                    | COMP CompOp
>                                   | JUMP Label
>          		                    | JUMPZ Label
>          		                    | LABEL Label
>                                   | FUNC Name 
>                                   | FEND
>                                   | CALL Name       
>                                   | STOP         
>		                                deriving Show
> 
> type Label 		            =  Int

-----------------------------------------------------------------------------------------------------
 
 EXECUTER 
 
Almost entirely from AFP at this stage a simple compiler for executing the code we have will update later on

This takes a list of machine code instructions and executes them.

> exec                          :: Code -> Number
> exec c                        = exec'  c 0 [] []


This is the function that actually does stuff

> exec'                         :: Code -> Int -> Stack -> Mem -> Number
> exec' c pc s m                =      
>                                       if(length c) <= pc then (Integer 0)
>                                       else 
>                                           case c !! pc of 
>                                               PUSH n      -> exec' c (pc+1) (push n s) m
>                                               PUSHV v     -> exec' c (pc+1) (pushv v m s) m
>                                               POP v       -> exec' c (pc+1) (pop s) (assignVariable v (head s) m)
>                                               DO o        -> exec' c (pc+1) (operation o s) m
>                                               COMP o      -> exec' c (pc+1) (comparison o s) m 
>                                               LABEL l     -> exec' c (pc+1) s m
>                                               JUMP l      -> exec' c (jump c l) s m
>                                               JUMPZ l     -> exec' c (jumpz c s l pc) (popz s) m
>                                               CALL n      -> handleCall c pc s m
>                                               STOP        -> (head s)
>                                               



STACK FUCNTIONS

This is a series of functions that handle the stack


push onto top of stack an integer

> push                          :: Number -> Stack -> Stack
> push (n) s                    =  [n] ++ s


push variable onto top of the stack

> pushv                         :: Name ->  Mem -> Stack -> Stack
> pushv v m s                   =  push (getVariable v m) s


pop an integer from the stack and places it into memory under that variable name

> pop                           :: Stack -> Stack
> pop s                         = tail s


Perform an operation on the first two things of the stack and leave the result on top

> operation                     :: ArthOp -> Stack -> Stack
> operation o s                 =   case o of 
>                                       ADD -> push (Integer (v2+v1)) ns 
>                                       SUB -> push (Integer (v2-v1)) ns
>                                       MUL -> push (Integer (v2*v1)) ns 
>                                       DIV -> push (Integer (div v2 v1)) ns
>                                   where
>                                       v1 = getNumber (head s)
>                                       v2 = getNumber (head (tail s))
>                                       ns = (pop (pop s))

Perform a comparison on the first two things on the stack (leave a 1 on top if true and 0 if false)

> comparison                    ::  CompOp -> Stack -> Stack
> comparison o s                =   push (comparison' o v1 v2) ns 
>                                       where
>                                           v2 = getNumber (head s)
>                                           v1 = getNumber (head (tail s))
>                                           ns = (pop (pop s))
>
> comparison'                   :: CompOp -> Int -> Int -> Number 
> comparison' o v1 v2           =   case o of
>                                       EQU		-> if v1 == v2  then t else f    
>                                       NEQ     -> if v1 == v2  then f else t
>                                       GEQ     -> if v1 >= v2  then t else f
>                                       LEQ     -> if v1 <= v2  then t else f
>                                       GRT     -> if v1 > v2   then t else f
>                                       LET     -> if v1 < v2   then t else f                                       
>                                   where 
>                                       t = Integer 1
>                                       f = Integer 0
                                                           

pops the head if it is 0 if not it leaves it be 

> popz                          :: Stack -> Stack
> popz s                
>                               | top == 0          = pop s
>                               | otherwise         = s
>                                   where
>                                       top = getNumber (head s)                

MEMORY FUNCTIONS

Functions that deal with memory


This function assigns an integer to a variable

> assignVariable                :: Name -> Number -> Mem -> Mem
> assignVariable  v n m         = (deleteVariable v m) ++ [(v, n)]


Get the variable from memory, if not assigned already just assume that variable is 0 

> getVariable                   :: Name -> Mem -> Number
> getVariable v ms        
>                               | null memV     = Integer 0 
>                               | otherwise     = snd (head memV)
>                                   where 
>                                       memV = [ m | m <- ms, fst m == v] 

Checks that the variable isn't being reassigned and if it is deletes that variable

> deleteVariable                :: Name -> Mem -> Mem
> deleteVariable v ms           = [ m | m <- ms, fst m /= v] 


JUMP FUNCTIONS

All the functions that deal with jumping arounbd our code

> jump                          ::  Code -> Int -> Int
> jump cs l                     =   case elemIndex True [ isLabel c l | c <- cs] of 
>                                       Just n      -> n
>                                       Nothing     -> 0     --Should never happen


deals with jumpz

> jumpz                         :: Code -> Stack -> Int -> Int -> Int
> jumpz c s l pc
>                               | head s == (Integer 0)     = jump c l
>                               | otherwise                 = (pc + 1)

returns true if is the label we are looking for

> isLabel                       :: Inst -> Int -> Bool
> isLabel c l                   =   case c of 
>                                       LABEL m         ->  (m == l) 
>                                       otherwise       ->  False  

stops the program, just returns memory at the moment 

> halt 							:: Mem -> Mem
> halt m						= m 


CALL Functions 

These functions deal with function calls

> handleCall                    :: Code -> Int -> Stack -> Mem -> Name -> Number
> handleCall c pc s m n         = exec' c (pc+1) (push fc s) m
>                                   where
>                                       fc = exec (getFunction c n [] False)


Searches through code and returns a function's code, ct signifies if is currently cutting the code

TODO: Make more efficent

> getFunction                   :: Code -> Name -> Code -> Bool -> Code 
> getFunction (c:cs) n fs ct    =   if n then
>                                       if (c == FEND) then fs
>                                       else getFunction cs n (c:fs) True                                            
>                                   else  
>                                       if (c == (FUNC n)) then getFunction cs n fs True 

--------------------------------------------------------------------------------

TYPE FUNCTIONS 

> getNumber                     :: Number -> Int
> getNumber n                   = getInt n

> getInt                        :: Number -> Int
> getInt (Integer n)            = n

> getDouble                     :: Number -> Double
> getDouble (Double n)          = n 

> isInt                         :: Number -> Bool
> isInt (Integer n)             = True
> isInt _                       = False 

> isDouble                      :: Number -> Bool
> isDouble (Double n)           = True
> isDouble _                    = False

> addNumbers                    :: Number -> Number -> Number   -- (x + y)
> addNumbers x y                =   if (isInt x) then Integer ((getInt x) + (getInt y)) 
>                                   else Double ((getDouble x) + (getDouble y))

> subNumbers                    :: Number -> Number -> Number   -- (x - y)
> subNumbers x y                =   if (isInt x) then Integer ((getInt x) - (getInt y))
>                                   else Double ((getDouble x) - (getDouble y)) 

Need to be carefull with divide because of how Go deals with int divison compared to haskell

> divideNumbers			        :: Number -> Number -> Number   -- (x / y)
> divideNumbers x y             =   if (isInt x) then (divideInt (getInt x) (getInt y))
>                                   else (divideDouble (getDouble x) (getDouble y))

> divideInt                     :: Int -> Int -> Number 
> divideInt x y                 = Integer (quot x y) 

> divideDouble                  :: Double -> Double -> Number
> divideDouble x y              = Double (x / y)

> multiplyNumbers               :: Number -> Number -> Number   -- (x * y)
> multiplyNumbers x y           =   if (isInt x) then Integer ((getInt x) * (getInt y))
>                                   else Double ((getDouble x) * (getDouble y))

--------------------------------------------------------------------------------
