
> import Data.List
> import Data.List.Split 

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
>                                   | Empty                                     -- Allows blank Prog may be useful 
>                                   | Return (Maybe Expr)                       
>                                   | Func Name [Name] Prog                     -- list of names is names of arguments
>                                   | Main Prog                                 -- gives us the main function to be run
>		                                deriving Show


Prog helpers, these are used as extensions to the Prog data structure 

> data ElseIfCase'              = Case Expr Prog                                --used for switch cases and else if 
>                                   deriving Show

EXPRESSION IR

> data Expr  		            =  Val Number 
>                                   | Var Name 
>                                   | ExprApp ArthOp Expr Expr 
>                                   | CompApp CompOp Expr Expr
>                                   | CondApp CondOp Expr Expr 
>                                   | FuncCall Name [Expr]
>                                       deriving Show

For Arthimetic Operations

> data ArthOp    		        =  ADD | SUB | MUL | DIV                        
>			                        deriving (Show, Eq)

For Comparisons

> data CompOp                   = EQU | NEQ | GEQ | LEQ | GRT | LET             
>			                        deriving (Show, Eq)    

For Conditionals, still to be finished 

> data CondOp                   = AND | OR 
>			                        deriving (Show, Eq)  

Numerical Types, currently treating booleans as integers, where 0 is false and 1 is true 

> data Number                   = Integer Int | Double Double
>			                        deriving (Show, Eq)

Name defintion

> type Name                     = String 


-----------------------------------------------------------------------------------------------------

TEST CASES

-----------------------------------------------------------------------------------------------------

COMPILER

Converts program into machine code

> comp                          :: Prog -> Code
> comp p                        =  fst (apply (comp' p) (0))


Actually compiles to machine code  

> comp'                         :: Prog -> ST Code
> comp' (Main p1)               = (mainDealer p1)
> comp' (Func n vs p1)          = (functionDealer n vs p1) 
> comp' (Seqn cs)               = (sequenceDealer cs)
> comp' (Assign n e)            = return (expression e ++ [POP n])
> comp' (If c p)				= (ifDealer c p)
> comp' (IfElse c p1 p2)        = (ifElseDealer c p1 p2)
> comp' (ElseIf c p1 cs p2)     = (elseIfDealer c p1 cs p2)
> comp' (While e p3)            = (whileDealer e p3)
> comp' (Return e)              = (returnDealer e)          
> comp' (Empty)                 = return []  

Deals with Expr

> expression                    :: Expr -> Code
> expression (FuncCall n es)    = (callDealer n es)
> expression (Val i)            = [PUSH i]
> expression (Var v)            = [PUSHV v]
> expression (ExprApp o x y)    = expression x ++ expression y ++ [DO o] 
> expression (CompApp o x y)    = expression x ++ expression y ++ [COMP o]


Deal with a list of Expr

> multExpression                :: [Expr] -> Code
> multExpression []             = []
> multExpression (e:es)         = expression e ++ multExpression es             


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

> returnDealer					:: Maybe Expr -> ST Code
> returnDealer e				=   case e of
>                                       Just exp    -> return (expression exp ++ [RSTOP])    
>                                       Nothing     -> return ([STOP])       

Deals With Sequences

> sequenceDealer                :: [Prog] -> ST Code 
> sequenceDealer []             = return []
> sequenceDealer (c:cs)         =   do  head    <- comp' c
>                                       tail    <- comp' (Seqn cs)
>                                       return (head ++ tail)  

Deals with Functions, assumes that ever function contains a return will be done at parsing level

> functionDealer                :: Name -> [Name] -> Prog ->  ST Code
> functionDealer n vs p1        =   do  p1code  <- comp' p1
>                                       return ([FUNC n] ++ [ POP v | v <-(reverse vs)] ++ p1code ++ [FEND])

Deals with function call puts whatever it is ontop of the stack if there is a return statement

> callDealer                    :: Name -> [Expr] -> Code
> callDealer n []               = [CALL n 0] 
> callDealer n (es)             = multExpression es ++ [CALL n (length es)] 

Deals with setting up the Main function and all the code to be run

> mainDealer                    :: Prog -> ST Code
> mainDealer p1                 =   do  p1Code  <- comp' p1 
>                                       return ([MAIN] ++ p1Code ++ [END]) 


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
>                                   | CALL Name Int                       
>                                   | STOP                      --Used for returning nothing
>                                   | RSTOP                     --Used to return a variable from a function
>                                   | MAIN
>                                   | END                       --Signifies the end of the main function         
>		                                deriving (Show, Eq)
> 
> type Label 		            =  Int

-----------------------------------------------------------------------------------------------------
 
 EXECUTER 
 
Almost entirely from AFP at this stage a simple compiler for executing the code we have will update later on

This takes a list of machine code instructions and executes them.

> exec                          :: Code -> Maybe Number
> exec c                        = exec' c c 0 [] []


This is the function that actually executes the code, c is the intial code, ec is what you're executing

> exec'                         :: Code -> Code ->  Int -> Stack -> Mem -> Maybe Number
> exec' c ec pc s m             =   case ec !! pc of 
>                                       MAIN        -> exec' c ec (pc+1) s m
>                                       FUNC n      -> exec' c ec (pc+1) s m        
>                                       PUSH n      -> exec' c ec (pc+1) (push n s) m
>                                       PUSHV v     -> exec' c ec (pc+1) (pushv v m s) m
>                                       POP v       -> exec' c ec (pc+1) (pop s) (assignVariable v (head s) m)
>                                       DO o        -> exec' c ec (pc+1) (operation o s) m
>                                       COMP o      -> exec' c ec (pc+1) (comparison o s) m 
>                                       LABEL l     -> exec' c ec (pc+1) s m
>                                       JUMP l      -> exec' c ec (jump c l) s m
>                                       JUMPZ l     -> exec' c ec (jumpz c s l pc) (pop s) m
>                                       (CALL n a)  -> (handleCall c ec pc s m n a)  
>                                       RSTOP       -> Just (head s)
>                                       STOP        -> Nothing
>                                       END         -> Nothing 
>                                       FEND        -> exec' c ec (pc+1) s m     
         
Returns the stack, used for function calls to managing stack frames and make sure variables,
don't get lost. Does the same as exec' but instead returns the stack.          
         
> stackExec                     :: Code -> Stack
> stackExec c                   = stackExec' c c 0 [] []              

> stackExec'                    :: Code -> Code -> Int -> Stack -> Mem -> Stack
> stackExec' c ec pc s m        =   case ec !! pc of 
>                                       MAIN        -> stackExec' c ec (pc+1) s m
>                                       FUNC n      -> stackExec' c ec (pc+1) s m        
>                                       PUSH n      -> stackExec' c ec (pc+1) (push n s) m
>                                       PUSHV v     -> stackExec' c ec (pc+1) (pushv v m s) m
>                                       POP v       -> stackExec' c ec (pc+1) (pop s) (assignVariable v (head s) m)
>                                       DO o        -> stackExec' c ec (pc+1) (operation o s) m
>                                       COMP o      -> stackExec' c ec (pc+1) (comparison o s) m 
>                                       LABEL l     -> stackExec' c ec (pc+1) s m
>                                       JUMP l      -> stackExec' c ec (jump ec l) s m
>                                       JUMPZ l     -> stackExec' c ec (jumpz ec s l pc) (pop s) m
>                                       (CALL n a)  -> (handleCallReStack c ec pc s m n a)  
>                                       RSTOP       -> s
>                                       STOP        -> s
>                                       END         -> s 
>                                       FEND        -> stackExec' c ec (pc+1) s m
         


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
>                                       ADD -> push (addNumbers v2 v1) ns 
>                                       SUB -> push (subNumbers v2 v1) ns
>                                       MUL -> push (mulNumbers v2 v1) ns 
>                                       DIV -> push (divNumbers v2 v1) ns
>                                   where
>                                       v1 = head s
>                                       v2 = head (tail s)
>                                       ns = (pop (pop s))

Perform a comparison on the first two things on the stack (leave a 1 on top if true and 0 if false)

> comparison                    ::  CompOp -> Stack -> Stack
> comparison o s                =   push (comparison' o v1 v2) ns 
>                                       where
>                                           v2 = (head s)
>                                           v1 = (head (tail s))
>                                           ns = (pop (pop s))
>
> comparison'                   :: CompOp -> Number -> Number -> Number 
> comparison' o v1 v2           =   case o of
>                                       EQU		-> if isNumEqu v1 v2  then t else f    
>                                       NEQ     -> if isNumNeq v1 v2  then f else t
>                                       GEQ     -> if isNumGeq v1 v2  then t else f
>                                       LEQ     -> if isNumLeq v1 v2  then t else f
>                                       GRT     -> if isNumGrt v1 v2  then t else f
>                                       LET     -> if isNumLet v1 v2  then t else f                                       
>                                   where 
>                                       t = Integer 1
>                                       f = Integer 0
                                                           

pops the head if it is 0 if not it leaves it be 

> popz                          :: Stack -> Stack
> popz s                
>                               | top == 0          = pop s
>                               | otherwise         = s
>                                   where
>                                       top = getInt (head s)                


Multiple pop, pops the required number of things

> multiplePop                   :: Stack -> Int -> Stack
> multiplePop ss 0              = ss
> multiplePop ss n              = multiplePop (pop ss) (n-1)   
 

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

These functions deal with function calls, if returns something put ontop off stack otherwise do 

> handleCall                    :: Code -> Code -> Int -> Stack -> Mem -> Name -> Int -> Maybe Number
> handleCall c ec pc s m n a    =   exec' c ec (pc+1) fs m   
>                                   where
>                                       fs      = stackExec' c fCode 0 s m
>                                       fCode   = (getFunction c n [] False False)
>                 

                      
Returns Stack, used for nested and recursive function calls

> handleCallReStack             :: Code -> Code-> Int -> Stack -> Mem -> Name -> Int -> Stack
> handleCallReStack c ec pc s m n a
>                               = stackExec' c ec (pc+1) (fRun) m
>                                   where
>                                       fRun    = stackExec' c fCode 0 s m
>                                       fCode   = (getFunction c n [] False False)
> 

  
Searches through code and returns a function's code, ct signifies if is currently cutting the code

> getFunction                   :: Code -> Name -> Code -> Bool -> Bool -> Code 
> getFunction (c:cs) n fs ct mt =   if ct then
>                                       if (c == FEND) then (reverse (c:fs))
>                                       else getFunction cs n (c:fs) True mt                                           
>                                   else  
>                                       if (c == (FUNC n)) then getFunction cs n (c:fs) True mt
>                                       else getFunction cs n fs False mt 
>                                   

--------------------------------------------------------------------------------

TYPE FUNCTIONS 

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

Comparison Operations

> isNumEqu                      :: Number -> Number -> Bool
> isNumEqu x y                  | (isInt x)         = (getInt x) == (getInt y) 
>                               | otherwise         = (getDouble x) == (getDouble y)

> isNumNeq                      :: Number -> Number -> Bool
> isNumNeq x y                  | (isInt x)         = (getInt x) /= (getInt y) 
>                               | otherwise         = (getDouble x) /= (getDouble y)

> isNumGeq                      :: Number -> Number -> Bool
> isNumGeq x y                  | (isInt x)         = (getInt x) >= (getInt y) 
>                               | otherwise         = (getDouble x) >= (getDouble y)

> isNumLeq                      :: Number -> Number -> Bool
> isNumLeq x y                  | (isInt x)         = (getInt x) <= (getInt y) 
>                               | otherwise         = (getDouble x) <= (getDouble y)

> isNumGrt                      :: Number -> Number -> Bool
> isNumGrt x y                  | (isInt x)         = (getInt x) > (getInt y) 
>                               | otherwise         = (getDouble x) > (getDouble y)

> isNumLet                      :: Number -> Number -> Bool
> isNumLet x y                  | (isInt x)         = (getInt x) < (getInt y) 
>                               | otherwise         = (getDouble x) < (getDouble y)

Arthimetic opperations

> addNumbers                    :: Number -> Number -> Number   -- (x + y)
> addNumbers x y                =   if (isInt x) then Integer ((getInt x) + (getInt y)) 
>                                   else Double ((getDouble x) + (getDouble y))

> subNumbers                    :: Number -> Number -> Number   -- (x - y)
> subNumbers x y                =   if (isInt x) then Integer ((getInt x) - (getInt y))
>                                   else Double ((getDouble x) - (getDouble y)) 

Need to be carefull with divide because of how Go deals with int divison compared to haskell

> divNumbers			        :: Number -> Number -> Number   -- (x / y)
> divNumbers x y                =   if (isInt x) then (divideInt (getInt x) (getInt y))
>                                   else (divideDouble (getDouble x) (getDouble y))

> divideInt                     :: Int -> Int -> Number 
> divideInt x y                 = Integer (quot x y) 


> divideDouble                  :: Double -> Double -> Number
> divideDouble x y              = Double (x / y)

> mulNumbers                    :: Number -> Number -> Number   -- (x * y)
> mulNumbers x y                =   if (isInt x) then Integer ((getInt x) * (getInt y))
>                                   else Double ((getDouble x) * (getDouble y))

--------------------------------------------------------------------------------
